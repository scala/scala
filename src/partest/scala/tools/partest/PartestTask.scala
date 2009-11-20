/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest

import scala.actors.Actor._

import java.io.File
import java.net.URLClassLoader

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
  private var scriptFiles: Option[FileSet] = None
  private var shootoutFiles: Option[FileSet] = None
  private var scalapFiles: Option[FileSet] = None
  private var errorOnFailed: Boolean = false
  private var scalacOpts: Option[String] = None
  private var timeout: Option[String] = None
  private var debug = false

  private def getFiles(fileSet: Option[FileSet]): Array[File] =
    if (fileSet.isEmpty) Array()
    else {
      val files = fileSet.get
      files.getDirectoryScanner(getProject).getIncludedFiles map {
       fs => new File(files.getDir(getProject), fs)
      }
    }

  private def getFilesAndDirs(fileSet: Option[FileSet]): Array[File] =
    if (!fileSet.isEmpty) {
      val files = fileSet.get
      val fileTests = getFiles(fileSet)
      val dir = files.getDir(getProject)
      val dirTests = dir.listFiles(new java.io.FileFilter {
        def accept(file: File) =
          file.isDirectory &&
          (!file.getName().equals(".svn")) &&
          (!file.getName().endsWith(".obj"))
      })
      (dirTests ++ fileTests).toArray
    }
    else
      Array()

  private def getPosFiles       = getFilesAndDirs(posFiles)
  private def getNegFiles       = getFiles(negFiles)
  private def getRunFiles       = getFiles(runFiles)
  private def getJvmFiles       = getFilesAndDirs(jvmFiles)
  private def getResidentFiles  = getFiles(residentFiles)
  private def getScriptFiles    = getFiles(scriptFiles)
  private def getShootoutFiles  = getFiles(shootoutFiles)
  private def getScalapFiles    = getFiles(scalapFiles)

  override def execute() {
    if (debug)
      System.setProperty("partest.debug", "true")

    if (classpath.isEmpty)
      error("Mandatory attribute 'classpath' is not set.")

    val scalaLibrary =
      (classpath.get.list map { fs => new File(fs) }) find { f =>
        f.getName match {
          case "scala-library.jar" => true
          case "library" if (f.getParentFile.getName == "classes") => true
          case _ => false
        }
      }

    if (scalaLibrary.isEmpty)
      error("Provided classpath does not contain a Scala library.")

    val classloader = this.getClass.getClassLoader

    val antRunner: AnyRef =
      classloader.loadClass("scala.tools.partest.nest.AntRunner").newInstance().asInstanceOf[AnyRef]
    val antFileManager: AnyRef =
      antRunner.getClass.getMethod("fileManager", Array[Class[_]](): _*).invoke(antRunner, Array[Object](): _*)

    val runMethod =
      antRunner.getClass.getMethod("reflectiveRunTestsForFiles", Array(classOf[Array[File]], classOf[String]): _*)

    def runTestsForFiles(kindFiles: Array[File], kind: String): (Int, Int) = {
      val result = runMethod.invoke(antRunner, Array(kindFiles, kind): _*).asInstanceOf[Int]
      (result >> 16, result & 0x00FF)
    }

    def setFileManagerBooleanProperty(name: String, value: Boolean) {
      val setMethod =
        antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[Boolean]): _*)
      setMethod.invoke(antFileManager, Array(java.lang.Boolean.valueOf(value)).asInstanceOf[Array[Object]]: _*)
    }

    def setFileManagerStringProperty(name: String, value: String) {
      val setMethod =
        antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[String]): _*)
      setMethod.invoke(antFileManager, Array(value).asInstanceOf[Array[Object]]: _*)
    }

    setFileManagerBooleanProperty("showDiff", showDiff)
    setFileManagerBooleanProperty("showLog", showLog)
    setFileManagerBooleanProperty("failed", runFailed)
    if (!javacmd.isEmpty)
      setFileManagerStringProperty("JAVACMD", javacmd.get.getAbsolutePath)
    if (!javaccmd.isEmpty)
      setFileManagerStringProperty("JAVAC_CMD", javaccmd.get.getAbsolutePath)
    setFileManagerStringProperty("CLASSPATH", classpath.get.list.mkString(File.pathSeparator))
    setFileManagerStringProperty("LATEST_LIB", scalaLibrary.get.getAbsolutePath)
    if (!scalacOpts.isEmpty)
      setFileManagerStringProperty("SCALAC_OPTS", scalacOpts.get)
    if (!timeout.isEmpty)
      setFileManagerStringProperty("timeout", timeout.get)

    type TFSet = (Array[File], String, String)
    val testFileSets = List(
      (getPosFiles, "pos", "Compiling files that are expected to build"),
      (getNegFiles, "neg", "Compiling files that are expected to fail"),
      (getRunFiles, "run", "Compiling and running files"),
      (getJvmFiles, "jvm", "Compiling and running files"),
      (getResidentFiles, "res", "Running resident compiler scenarii"),
      (getScriptFiles, "script", "Running script files"),
      (getShootoutFiles, "shootout", "Running shootout tests"),
      (getScalapFiles, "scalap", "Running scalap tests")
    )

    def runSet(set: TFSet): (Int, Int) = {
      val (files, name, msg) = set
      if (files.isEmpty) (0, 0)
      else {
        log(msg)
        runTestsForFiles(files, name)
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = _results map (_._1) sum
    val allFailures = _results map (_._2) sum

    def f = if (errorOnFailed && allFailures > 0) error(_) else log(_: String)
    def s = if (allFailures > 1) "s" else ""
    val msg =
      if (allFailures > 0) "Test suite finished with %d case%s failing.".format(allFailures, s)
      else if (allSuccesses == 0) "There were no tests to run."
      else "Test suite finished with no failures."

    f(msg)
  }
}
