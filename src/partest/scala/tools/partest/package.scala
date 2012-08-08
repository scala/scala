/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 */

package scala.tools

import java.io.{ FileNotFoundException, File => JFile }
import nsc.io.{ Path, Directory, File => SFile }
import util.{ PathResolver }
import nsc.Properties.{ propOrElse, propOrNone, propOrEmpty }
import scala.sys.process.javaVmArguments
import java.util.concurrent.Callable

package partest {
  class TestState {
    def isOk      = this eq TestState.Ok
    def isFail    = this eq TestState.Fail
    def isTimeout = this eq TestState.Timeout
  }
  object TestState {
    val Ok      = new TestState
    val Fail    = new TestState
    val Timeout = new TestState
  }
}

package object partest {
  import nest.NestUI

  implicit private[partest] def temporaryPath2File(x: Path): JFile = x.jfile
  implicit private[partest] def temporaryFile2Path(x: JFile): Path = Path(x)

  implicit lazy val postfixOps = language.postfixOps
  implicit lazy val implicitConversions = language.implicitConversions

  def timed[T](body: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val result = body
    val t2 = System.currentTimeMillis

    (result, t2 - t1)
  }

  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }

  def path2String(path: String) = file2String(new JFile(path))
  def file2String(f: JFile) =
    try SFile(f).slurp()
    catch { case _: FileNotFoundException => "" }

  def basename(name: String): String = Path(name).stripExtension

  def resultsToStatistics(results: Iterable[(_, TestState)]): (Int, Int) = {
    val (files, failures) = results map (_._2 == TestState.Ok) partition (_ == true)
    (files.size, failures.size)
  }

  def vmArgString = javaVmArguments.mkString(
    "Java VM started with arguments: '",
    " ",
    "'"
  )

  def allPropertiesString = {
    import collection.JavaConversions._
    System.getProperties.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString ""
  }

  def showAllJVMInfo() {
    NestUI.verbose(vmArgString)
    NestUI.verbose(allPropertiesString)
  }

  def isPartestDebug: Boolean =
    propOrEmpty("partest.debug") == "true"
}
