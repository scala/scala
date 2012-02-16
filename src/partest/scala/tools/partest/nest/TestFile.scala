/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{ File => JFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io._
import scala.util.Properties.{ propIsSet, propOrElse, setProp }

trait TestFileCommon {
  def file: JFile
  def kind: String

  val dir       = file.toAbsolute.parent
  val fileBase  = file.stripExtension
  val flags     = dir / (fileBase + ".flags") ifFile (f => f.slurp().trim)

  lazy val objectDir = dir / (fileBase + "-" + kind + ".obj") createDirectory true
  def setOutDirTo = objectDir
}

abstract class TestFile(val kind: String) extends TestFileCommon {
  def file: JFile
  def fileManager: FileManager

  def defineSettings(settings: Settings, setOutDir: Boolean) = {
    settings.classpath append dir.path
    if (setOutDir)
      settings.outputDirs setSingleOutput setOutDirTo.path

    // adding code.jar to the classpath (to provide Code.lift services for reification tests)
    settings.classpath prepend PathSettings.srcCodeLib.toString
    if (propIsSet("java.class.path")) setProp("java.class.path", PathSettings.srcCodeLib.toString + ";" + propOrElse("java.class.path", ""))

    // have to catch bad flags somewhere
    (flags forall (f => settings.processArgumentString(f)._1)) && {
      settings.classpath append fileManager.CLASSPATH
      true
    }
  }

  override def toString(): String = "%s %s".format(kind, file)
}

case class PosTestFile(file: JFile, fileManager: FileManager) extends TestFile("pos")
case class NegTestFile(file: JFile, fileManager: FileManager) extends TestFile("neg")
case class RunTestFile(file: JFile, fileManager: FileManager) extends TestFile("run")
case class BuildManagerTestFile(file: JFile, fileManager: FileManager) extends TestFile("bm")
case class ScalaCheckTestFile(file: JFile, fileManager: FileManager) extends TestFile("scalacheck")
case class JvmTestFile(file: JFile, fileManager: FileManager) extends TestFile("jvm")
case class ShootoutTestFile(file: JFile, fileManager: FileManager) extends TestFile("shootout") {
  override def setOutDirTo = file.parent
}
case class ScalapTestFile(file: JFile, fileManager: FileManager) extends TestFile("scalap") {
  override def setOutDirTo = file.parent
}
case class SpecializedTestFile(file: JFile, fileManager: FileManager) extends TestFile("specialized") {
  override def defineSettings(settings: Settings, setOutDir: Boolean): Boolean = {
    super.defineSettings(settings, setOutDir) && {
      // add the instrumented library version to classpath
      settings.classpath prepend PathSettings.srcSpecLib.toString
      // @partest maintainer: if we use a custom Scala build (specified via --classpath)
      // then the classes provided by it will come earlier than instrumented.jar in the resulting classpath
      // this entire classpath business needs a thorough solution
      if (propIsSet("java.class.path")) setProp("java.class.path", PathSettings.srcSpecLib.toString + ";" + propOrElse("java.class.path", ""))
      true
    }
  }
}
case class PresentationTestFile(file: JFile, fileManager: FileManager) extends TestFile("presentation")
case class AntTestFile(file: JFile, fileManager: FileManager) extends TestFile("ant")
