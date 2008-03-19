/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
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

  def addConfiguredPosTests(input: FileSet): Unit =
    posFiles = Some(input)

  def addConfiguredNegTests(input: FileSet): Unit =
    negFiles = Some(input)

  def addConfiguredRunTests(input: FileSet): Unit =
    runFiles = Some(input)

  def addConfiguredResidentTests(input: FileSet): Unit =
    residentFiles = Some(input)

  def setClasspath(input: Path): Unit =
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)

  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  def setClasspathref(input: Reference): Unit =
    createClasspath().setRefid(input)

  def setShowLog(input: Boolean): Unit =
    showLog = input

  def setShowDiff(input: Boolean): Unit =
    showDiff = input

  def setErrorOnFailed(input: Boolean): Unit =
    errorOnFailed = input

  private var classpath: Option[Path] = None
  private var javacmd: Option[File] = None
  private var showDiff: Boolean = false
  private var showLog: Boolean = false
  private var runFailed: Boolean = false
  private var posFiles: Option[FileSet] = None
  private var negFiles: Option[FileSet] = None
  private var runFiles: Option[FileSet] = None
  private var residentFiles: Option[FileSet] = None
  private var errorOnFailed: Boolean = false

  private def getPosFiles: Array[File] =
    if (!posFiles.isEmpty) {
      val files = posFiles.get
      (files.getDirectoryScanner(getProject).getIncludedFiles map { fs => new File(files.getDir(getProject), fs) })
    }
    else
      Array()

  private def getNegFiles: Array[File] =
    if (!negFiles.isEmpty) {
      val files = negFiles.get
      (files.getDirectoryScanner(getProject).getIncludedFiles map { fs => new File(files.getDir(getProject), fs) })
    }
    else
      Array()

  private def getRunFiles: Array[File] =
    if (!runFiles.isEmpty) {
      val files = runFiles.get
      (files.getDirectoryScanner(getProject).getIncludedFiles map { fs => new File(files.getDir(getProject), fs) })
    }
    else
      Array()

  private def getResidentFiles: Array[File] =
    if (!residentFiles.isEmpty) {
      val files = residentFiles.get
      (files.getDirectoryScanner(getProject).getIncludedFiles map { fs => new File(files.getDir(getProject), fs) })
    }
    else
      Array()


  override def execute(): Unit = {

    if (classpath.isEmpty)
      error("Mandatory attribute 'classpath' is not set.")

    val scalaLibrary =
      (classpath.get.list map { fs => new File(fs) }) find { f =>
        f.getName match {
          case "scala-library.jar" => true
          case "lib" if (f.getParentFile.getName == "library") => true
          case _ => false
        }
      }

    if (scalaLibrary.isEmpty)
      error("Provided classpath does not contain a Scala library.")

    val classloader = this.getClass.getClassLoader

    val antRunner: AnyRef =
      classloader.loadClass("scala.tools.partest.nest.AntRunner").newInstance().asInstanceOf[AnyRef]
    val antFileManager: AnyRef =
      antRunner.getClass.getMethod("fileManager", Array()).invoke(antRunner, Array())

    val runMethod =
      antRunner.getClass.getMethod("reflectiveRunTestsForFiles", Array(classOf[Array[File]], classOf[String]))

    def runTestsForFiles(kindFiles: Array[File], kind: String): (Int, Int) = {
      val result = runMethod.invoke(antRunner, Array(kindFiles, kind)).asInstanceOf[Int]
      (result >> 16, result & 0x00FF)
    }

    def setFileManagerBooleanProperty(name: String, value: Boolean) = {
      val setMethod =
        antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[Boolean]))
      setMethod.invoke(antFileManager, Array(java.lang.Boolean.valueOf(value)))
    }

    def setFileManagerStringProperty(name: String, value: String) = {
      val setMethod =
        antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[String]))
      setMethod.invoke(antFileManager, Array(value))
    }

    setFileManagerBooleanProperty("showDiff", showDiff)
    setFileManagerBooleanProperty("showLog", showLog)
    setFileManagerBooleanProperty("failed", runFailed)
    if (!javacmd.isEmpty)
      setFileManagerStringProperty("JAVACMD", javacmd.get.getAbsolutePath)
    setFileManagerStringProperty("CLASSPATH", classpath.get.list.mkString(File.pathSeparator))
    setFileManagerStringProperty("LATEST_LIB", scalaLibrary.get.getAbsolutePath)

    var allSucesses: int = 0
    var allFailures: int = 0

    if (getPosFiles.size > 0) {
      log("Compiling files that are expected to build")
      val (successes, failures) = runTestsForFiles(getPosFiles, "pos")
      allSucesses += successes
      allFailures += failures
    }

    if (getNegFiles.size > 0) {
      log("Compiling files that are expected to fail")
      val (successes, failures) = runTestsForFiles(getNegFiles, "neg")
      allSucesses += successes
      allFailures += failures
    }

    if (getRunFiles.size > 0) {
      log("Compiling and running files")
      val (successes, failures) = runTestsForFiles(getRunFiles, "run")
      allSucesses += successes
      allFailures += failures
    }

    if (getResidentFiles.size > 0) {
      log("Running resident compiler scenarii")
      val (successes, failures) = runTestsForFiles(getResidentFiles, "res")
      allSucesses += successes
      allFailures += failures
    }

    if ((getPosFiles.size + getNegFiles.size + getRunFiles.size + getResidentFiles.size) == 0)
      log("There where no tests to run.")
    else if (allFailures == 0)
      log("Test suite finished with no failures.")
    else if (errorOnFailed)
      error("Test suite finished with " + allFailures + " case" + (if (allFailures > 1) "s" else "") + " failing.")
    else
      log("Test suite finished with " + allFailures + " case" + (if (allFailures > 1) "s" else "") + " failing.")

  }

}
