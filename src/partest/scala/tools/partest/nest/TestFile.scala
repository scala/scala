/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{ File => JFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.io._

abstract class TestFile(kind: String) {
  def file: JFile
  def fileManager: FileManager

  val dir = file.toAbsolute.parent
  val fileBase = file.stripExtension
  lazy val objectDir = dir / "%s-%s.obj".format(fileBase, kind) createDirectory true
  val flags: Option[String] = dir / "%s.flags".format(fileBase) ifFile { _.slurp().trim }

  def setOutDirTo = objectDir

  def defineSettings(settings: Settings, setOutDir: Boolean) = {
    settings appendToClasspath dir.path
    if (setOutDir)
      settings.outdir.value = setOutDirTo.path

    flags foreach (settings processArgumentString _)
    settings appendToClasspath fileManager.CLASSPATH
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
