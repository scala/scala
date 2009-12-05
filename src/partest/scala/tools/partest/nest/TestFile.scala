/* NEST (New Scala Test)
 * Copyright 2007-2009 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, BufferedReader, FileReader}
import scala.tools.nsc.Settings

class TestFile(kind: String, val file: File, val fileManager: FileManager, createOutDir: Boolean) {
  val dir = file.getParentFile
  val dirpath = dir.getAbsolutePath
  val fileBase: String = basename(file.getName)

  // @mutates settings
  protected def baseSettings(settings: Settings) {
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+dirpath
    if (createOutDir)
      settings.outdir.value = {
        val outDir = new File(dir, fileBase + "-" + kind + ".obj")
        if (!outDir.exists)
          outDir.mkdir()
        outDir.toString
      }

    // add additional flags found in 'testname.flags'
    val flagsFile = new File(dir, fileBase + ".flags")
    if (flagsFile.exists) {
      val reader = new BufferedReader(new java.io.FileReader(flagsFile))
      val flags = reader.readLine
      if (flags ne null)
        settings.parseParams(settings.splitParams(flags))
    }
  }

  def defineSettings(settings: Settings) {
    baseSettings(settings)
  }

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  override def toString(): String = kind+" "+file
}

case class PosTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("pos", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
  }
}

case class NegTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("neg", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
  }
}

case class RunTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("run", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
  }
}

case class ScalaCheckTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("scalacheck", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
  }
}

case class JvmTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("jvm", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
  }
}

case class ShootoutTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("shootout", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
    settings.outdir.value = file.getParent
  }
}

case class ScalapTestFile(override val file: File, override val fileManager: FileManager, createOutDir: Boolean) extends TestFile("scalap", file, fileManager, createOutDir) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = settings.classpath.value+
      File.pathSeparator+fileManager.CLASSPATH
    settings.outdir.value = file.getParent
  }
}
