/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.File
import scala.tools.nsc.Settings

class TestFile(kind: String, val file: File) {
  val dir = file.getParentFile
  val dirpath = dir.getAbsolutePath

  val fileBase: String = basename(file.getName)

  val logFile = new File(dir, fileBase + "-" + kind + ".log")
  val checkFile = {
    val chkFile = new File(dir, fileBase + ".check")
    if (chkFile.isFile)
      chkFile
    else
      new File(dir, fileBase + "-" + kind + ".check")
  }

  // @mutates settings
  protected def baseSettings(settings: Settings) {
    settings.classpath.value = dirpath
    settings.outdir.value = {
      val outDir = new File(dir, fileBase + "-" + kind + ".obj")
      if (!outDir.exists)
        outDir.mkdir()
      outDir.toString
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

case class PosTestFile(override val file: File) extends TestFile("pos", file)

case class NegTestFile(override val file: File) extends TestFile("neg", file)

case class RunTestFile(override val file: File) extends TestFile("run", file) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
  }
}

case class JvmTestFile(override val file: File) extends TestFile("jvm", file) {
  import FileManager.{CLASSPATH, EXT_CLASSPATH, PATH_SEP}

  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = CLASSPATH+PATH_SEP+EXT_CLASSPATH
    //println("settings.classpath.value="+settings.classpath.value)
  }
}

case class Jvm5TestFile(override val file: File) extends TestFile("jvm5", file) {
  import FileManager.{CLASSPATH, EXT_CLASSPATH, PATH_SEP}

  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = CLASSPATH+PATH_SEP+EXT_CLASSPATH
    settings.target.value = "jvm-1.5"
    //println("settings.classpath.value="+settings.classpath.value)
  }
}

case class ShootoutTestFile(override val file: File) extends TestFile("shootout", file) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = System.getProperty("EXT_CLASSPATH")
    //println("CLASSPATH="+settings.classpath.value)
  }
}
