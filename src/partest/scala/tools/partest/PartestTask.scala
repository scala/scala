/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools
package partest

import scala.actors.Actor._
import scala.util.Properties.setProp
import scala.tools.nsc.io
import io.{ Directory }
import nsc.Settings
import nsc.util.ClassPath
import util.PathResolver

import java.io.File
import java.net.URLClassLoader
import java.lang.reflect.Method

import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference, FileSet}

class PartestTask extends Task {

  def addConfiguredPosTests(input: FileSet) {
    posFiles = Some(input)
  }

  def addConfiguredNegTests(input: FileSet) {
    negFiles = Some(input)
  }

  def addConfiguredRunTests(input: FileSet) {
    runFiles = Some(input)
  }

  def addConfiguredJvmTests(input: FileSet) {
    jvmFiles = Some(input)
  }

  def addConfiguredResidentTests(input: FileSet) {
    residentFiles = Some(input)
  }

  def addConfiguredBuildManagerTests(input: FileSet) {
    buildManagerFiles = Some(input)
  }

  def addConfiguredScalacheckTests(input: FileSet) {
    scalacheckFiles = Some(input)
  }

  def addConfiguredScriptTests(input: FileSet) {
    scriptFiles = Some(input)
  }

  def addConfiguredShootoutTests(input: FileSet) {
    shootoutFiles = Some(input)
  }

  def addConfiguredScalapTests(input: FileSet) {
    scalapFiles = Some(input)
  }

  def setClasspath(input: Path) {
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)
  }

  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }

  def setShowLog(input: Boolean) {
    showLog = input
  }

  def setShowDiff(input: Boolean) {
    showDiff = input
  }

  def setErrorOnFailed(input: Boolean) {
    errorOnFailed = input
  }

  def setJavaCmd(input: File) {
    javacmd = Some(input)
  }

  def setJavacCmd(input: File) {
    javaccmd = Some(input)
  }

  def setScalacOpts(opts: String) {
    scalacOpts = Some(opts)
  }

  def setTimeout(delay: String) {
    timeout = Some(delay)
  }

  def setDebug(input: Boolean) {
    debug = input
  }

  def setJUnitReportDir(input: File) {
    jUnitReportDir = Some(input)
  }

  private var classpath: Option[Path] = None
  private var javacmd: Option[File] = None
  private var javaccmd: Option[File] = None
  private var showDiff: Boolean = false
  private var showLog: Boolean = false
  private var runFailed: Boolean = false
  private var posFiles: Option[FileSet] = None
  private var negFiles: Option[FileSet] = None
  private var runFiles: Option[FileSet] = None
  private var jvmFiles: Option[FileSet] = None
  private var residentFiles: Option[FileSet] = None
  private var buildManagerFiles: Option[FileSet] = None
  private var scalacheckFiles: Option[FileSet] = None
  private var scriptFiles: Option[FileSet] = None
  private var shootoutFiles: Option[FileSet] = None
  private var scalapFiles: Option[FileSet] = None
  private var errorOnFailed: Boolean = false
  private var scalacOpts: Option[String] = None
  private var timeout: Option[String] = None
  private var jUnitReportDir: Option[File] = None
  private var debug = false

  def fileSetToDir(fs: FileSet) = Directory(fs getDir getProject)
  def fileSetToArray(fs: FileSet): Array[io.Path] = {
    val root = fileSetToDir(fs)
    (fs getDirectoryScanner getProject).getIncludedFiles map (root / _)
  }

  private def getFiles(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) => fileSetToArray(fs) filterNot (_ hasExtension "log") map (_.jfile)
  }

  private def getFilesAndDirs(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) =>
      val fileTests = getFiles(Some(fs))
      val dirTests: Iterator[io.Path] = fileSetToDir(fs).dirs filterNot (x => (x hasExtension "svn") || (x hasExtension "obj"))
      val dirResult = dirTests.toList.toArray map (_.jfile)

      dirResult ++ fileTests
  }

  private def getPosFiles          = getFilesAndDirs(posFiles)
  private def getNegFiles          = getFilesAndDirs(negFiles)
  private def getRunFiles          = getFiles(runFiles)
  private def getJvmFiles          = getFilesAndDirs(jvmFiles)
  private def getResidentFiles     = getFiles(residentFiles)
  private def getBuildManagerFiles = getFilesAndDirs(buildManagerFiles)
  private def getScalacheckFiles   = getFiles(scalacheckFiles)
  private def getScriptFiles       = getFiles(scriptFiles)
  private def getShootoutFiles     = getFiles(shootoutFiles)
  private def getScalapFiles       = getFiles(scalapFiles)

  private def findMethod(target: AnyRef, name: String, types: Class[_]*): Method =
    target.getClass.getMethod(name, Array(types: _*): _*)

  private def invokeMethod[T](target: AnyRef, m: Method, args: AnyRef*): T =
    m.invoke(target, args: _*).asInstanceOf[T]

  private def invoke[T](target: AnyRef, name: String, args: Any*): T = {
    val boxed = args map (_.asInstanceOf[AnyRef])
    val m = findMethod(target, name, boxed map (_.getClass): _*)
    invokeMethod[T](target, m, boxed: _*)
  }

  override def execute() {
    if (isPartestDebug)
      setProp("partest.debug", "true")

    val classpath = this.classpath getOrElse error("Mandatory attribute 'classpath' is not set.")

    val scalaLibrary = {
      (classpath.list map { fs => new File(fs) }) find { f =>
        f.getName match {
          case "scala-library.jar" => true
          case "library" if (f.getParentFile.getName == "classes") => true
          case _ => false
        }
      }
    } getOrElse error("Provided classpath does not contain a Scala library.")

    val classloader = this.getClass.getClassLoader
    def load(name: String) = classloader.loadClass(name).newInstance().asInstanceOf[AnyRef]

    val antRunner       = load("scala.tools.partest.nest.AntRunner")
    val antFileManager  = invoke[AnyRef](antRunner, "fileManager")
    val runMethod       = findMethod(antRunner, "reflectiveRunTestsForFiles", classOf[Array[File]], classOf[String])

    def runTestsForFiles(kindFiles: Array[File], kind: String) =
      invokeMethod[Map[String, Int]](antRunner, runMethod, kindFiles, kind)

    def setFileManagerBooleanProperty(name: String, value: Boolean) {
      val setMethod = findMethod(antFileManager, name + "_$eq", classOf[Boolean])
      invokeMethod[Unit](antFileManager, setMethod, Boolean.box(value))
    }

    def setFileManagerStringProperty(name: String, value: String) {
      val setMethod = findMethod(antFileManager, name + "_$eq", classOf[String])
      invokeMethod[Unit](antFileManager, setMethod, value)
    }

    setFileManagerBooleanProperty("showDiff", showDiff)
    setFileManagerBooleanProperty("showLog", showLog)
    setFileManagerBooleanProperty("failed", runFailed)
    setFileManagerStringProperty("CLASSPATH", ClassPath.join(classpath.list: _*))
    setFileManagerStringProperty("LATEST_LIB", scalaLibrary.getAbsolutePath)

    javacmd foreach (x => setFileManagerStringProperty("JAVACMD", x.getAbsolutePath))
    javaccmd foreach (x => setFileManagerStringProperty("JAVAC_CMD", x.getAbsolutePath))
    scalacOpts foreach (x => setFileManagerStringProperty("SCALAC_OPTS", x))
    timeout foreach (x => setFileManagerStringProperty("timeout", x))

    type TFSet = (Array[File], String, String)
    val testFileSets = List(
      (getPosFiles, "pos", "Compiling files that are expected to build"),
      (getNegFiles, "neg", "Compiling files that are expected to fail"),
      (getRunFiles, "run", "Compiling and running files"),
      (getJvmFiles, "jvm", "Compiling and running files"),
      (getResidentFiles, "res", "Running resident compiler scenarii"),
      (getBuildManagerFiles, "buildmanager", "Running Build Manager scenarii"),
      (getScalacheckFiles, "scalacheck", "Running scalacheck tests"),
      (getScriptFiles, "script", "Running script files"),
      (getShootoutFiles, "shootout", "Running shootout tests"),
      (getScalapFiles, "scalap", "Running scalap tests")
    )

    def runSet(set: TFSet): (Int, Int, Iterable[String]) = {
      val (files, name, msg) = set
      if (files.isEmpty) (0, 0, List())
      else {
        log(msg)
        val results: Iterable[(String, Int)] = runTestsForFiles(files, name)
        val (succs, fails) = resultsToStatistics(results)

        val failed: Iterable[String] = results partialMap {
          case (path, 1)    => path + " [FAILED]"
          case (path, 2)    => path + " [TIMOUT]"
        }

        // create JUnit Report xml files if directory was specified
        jUnitReportDir foreach { d =>
          d.mkdir

          val report = testReport(name, results, succs, fails)
          scala.xml.XML.save(d.getAbsolutePath+"/"+name+".xml", report)
        }

        (succs, fails, failed)
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = _results map (_._1) sum
    val allFailures = _results map (_._2) sum
    val allFailedPaths = _results flatMap (_._3)

    def f = if (errorOnFailed && allFailures > 0) error(_) else log(_: String)
    def s = if (allFailures > 1) "s" else ""
    val msg =
      if (allFailures > 0)
        "Test suite finished with %d case%s failing:\n".format(allFailures, s)+
        allFailedPaths.mkString("\n")
      else if (allSuccesses == 0) "There were no tests to run."
      else "Test suite finished with no failures."

    f(msg)
  }
  def oneResult(res: (String, Int)) =
    <testcase name={res._1}>{
  	  res._2 match {
  	    case 0 => scala.xml.NodeSeq.Empty
        case 1 => <failure message="Test failed"/>
        case 2 => <failure message="Test timed out"/>
  	  }
  	}</testcase>

  def testReport(kind: String, results: Iterable[(String, Int)], succs: Int, fails: Int) =
    <testsuite name={kind} tests={(succs + fails).toString} failures={fails.toString}>
  	  <properties/>
  	  {
  	    results.map(oneResult(_))
  	  }
    </testsuite>
}
