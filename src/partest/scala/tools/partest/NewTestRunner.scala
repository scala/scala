package scala.tools.partest

import java.io.{File, FilenameFilter, FileInputStream, FileOutputStream,
                PrintStream}
import java.net.URI

import scala.tools.nsc.Settings

import utils.PrintMgr
import utils.PrintMgr._

/**
 * @author  Adriaan Moors, Thomas Hofer
 * @version 1.0
 */
class Test(val kind: String, val file: File) {
  val dir = file.getParentFile
  val dirpath = dir.getAbsolutePath
  protected def baseSettings(settings: Settings) {
    settings.classpath.value = dirpath
    settings.outdir.value = {
      var outDir = new File(dir, fileBase + "-" + kind + ".obj")
      outDir.mkdir
      outDir.toString
    }
    settings.deprecation.value = true
    settings.nowarnings.value = false
    settings.encoding.value = "iso-8859-1"
  }
  def defineSettings(settings: Settings) {
    baseSettings(settings)
  }
  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }
  val fileBase: String = basename(file.getName)
  val logFile = new File(dir, fileBase + "-" + kind + ".log")
  val checkFile = {
    val chkFile = new File(dir, fileBase + ".check")
    if (chkFile.isFile) // @P: in which case is it not a file?
      chkFile
    else
      new File(dir, fileBase + "-" + kind + ".check")
  }
}

case class PosTest(override val file: File) extends Test("pos", file)

object NewTestRunner {

  private val srcDir = {
    val dirname = System.getProperty("scalatest.cwd", "")
    val dir = if (dirname.isEmpty) { // guess
      val libDir = new File(new URI(classOf[Test].getResource("/").toString))
      val path = libDir.getAbsolutePath
      val parent = libDir.getParentFile
      val rootDir =
        if (path contains "quick") parent.getParentFile.getParentFile.getParentFile
        else if (path contains "dists") parent.getParentFile.getParentFile
        else parent
      new File(rootDir, "test" + File.separator + "files")
    } else
      new File(dirname)
    dir
  }

  private val testFiles = new collection.mutable.ListBuffer[File]

  private def go() {
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name endsWith ".scala"
    }
    def getFiles(kind: String, doCheck: Boolean): List[File] = {
      val dir = new File(srcDir, kind)
      if (dir.isDirectory) {
        if (! testFiles.isEmpty) {
          val dirpath = dir.getAbsolutePath
          val files = testFiles filter { _.getParentFile.getAbsolutePath == dirpath }
          files.toList
        } else if (doCheck)
          dir.listFiles(filter).toList
        else // skip
          Nil
      } else {
        println("Directory \"" + dir.getPath + "\" not found")
        Nil
      }
    }

    val posCheck = true
    val posFiles = getFiles("pos", posCheck)
    if (!posFiles.isEmpty) {
      printOutline("\nTesting compiler (on files whose compilation should succeed)\n")

      for (file <- posFiles) {
        val test = PosTest(file)

        // @P: why is this a var?
        var toCompile = List(file.getPath)

        var outDir = new File(test.dir, test.fileBase + "-" + test.kind + ".obj")
        if (!outDir.exists) {
          outDir.mkdir
          println(this+" "+"created "+outDir)
        } else {
          println(this+" "+"didn't have to create "+outDir)
        }
      }
    }
  }

  def main(args: Array[String]) {
    if (!srcDir.isDirectory) {
      println("Test directory \"" + srcDir.getAbsolutePath + "\" not found")
      exit(1)
    }
    printVerbose(srcDir.getAbsolutePath)
    if (args.length == 0)
      printUsage
    else {
      initialization(PrintMgr.MANY)
      go()
    }
  }

  private def printUsage {
    println("Usage: TestRunner [<options>] [<testfile> ..] [<resfile>]")
    println("    --pos          next files test a compilation success")
    println("    --neg          next files test a compilation failure")
    println("    --jvm          next files test the JVM backend")
    println("    --run          next files test the interpreter and all backends")
    println("    --shootout     ...")
    println("    --conservative ...")
    println("    --verbose      display progress information")
    println("    --version      output version information and exit")
    println
    println("Send bugs to <scala@listes.epfl.ch>")
    exit(1)
  }

  var verbose = false

  private def printVerbose(msg: String) {
    if (verbose) {
      printOutline("debug  : ")
      println(msg)
    }
  }
}
