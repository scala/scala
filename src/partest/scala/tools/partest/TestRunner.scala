/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{File, FilenameFilter, FileInputStream, FileOutputStream,
                PrintStream}

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
  val logFile: File = new File(dir, fileBase + "-" + kind + ".log")
  val checkFile: File = {
    var chkFile = new File(dir, fileBase + ".check")
    if (chkFile.isFile) {
      chkFile
    } else {
      new File(dir, fileBase + "-" + kind + ".check")
    }
  }
}

case class PosTest(override val file: File) extends Test("pos", file)
case class NegTest(override val file: File) extends Test("neg", file)
case class JVMTest(override val file: File) extends Test("jvm", file) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.target.value =
      if (dirpath endsWith "jvm5") "jvm-1.5" else "jvm-1.4"
    settings.classpath.value = System.getProperty("EXT_CLASSPATH")
    TestRunner.printVerbose("CLASSPATH="+settings.classpath.value)
  }
}
case class ShootoutTest(override val file: File) extends Test("shootout", file) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = System.getProperty("EXT_CLASSPATH")
    TestRunner.printVerbose("CLASSPATH="+settings.classpath.value)
  }
}

/**
 * @author  Stephane Micheloud
 * @version 1.0
 */
object TestRunner {
  private final val version = System.getProperty("java.version", "")
  private final val isJava5 = version matches "1.[5|6|7].*"

  private var posCheck = false
  private var negCheck = false
  private var jvmCheck = false
  private var runCheck = false
  private var shootoutCheck = false

  private var conservative = false
  private var verbose = false

  private val srcDir = {
    val dirname = System.getProperty("scalatest.cwd", "")
    val dir = if (dirname.isEmpty) { // guess
      val libDir = new File(classOf[Test].getResource("/").toURI)
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
  private val testDir = srcDir.getParentFile

  private var testFiles = new collection.mutable.ListBuffer[File]
  private val con = new PrintStream(Console.out)
  private var out = con

  private def createTestFile(file: File, suffix: String): File = {
    def getBaseName(f: File): String = {
      val name = f.getName
      val inx = name lastIndexOf '.'
      if (inx < 0) name else name.substring(0, inx)
    }
    def concat(outputFile: File, inputFiles: File*) {
      val out = new FileOutputStream(outputFile)
      for (f <- inputFiles) {
        val in = new FileInputStream(f)
        val buf = new Array[Byte](1024)
        var len = 0
        while (len != -1) {
          out.write(buf, 0, len)
          len = in.read(buf)
        }
        in.close
      }
      out.close
    }
    try {
      val parent = file.getParentFile
      val outDir = new File(parent, getBaseName(file) + "-" + suffix + ".obj")
      outDir.mkdir
      val testfile = new File(outDir, "test.scala")
      val runnerfile = new File(parent, file.getName + ".runner")
      concat(testfile, file, runnerfile)
      testfile
    }
    catch {
      case e: Exception =>
        println("Couldn't create test file for \"" + file.getPath + "\"")
        file
    }
  }

  private def go {
    val master = new MasterActor(testDir, out)
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

    master.start

    val posFiles = getFiles("pos", posCheck)
    if (! posFiles.isEmpty) {
      printOutline("\nTesting compiler (on files whose compilation should succeed)\n")
      for (file <- posFiles) master ! PosTest(file)
    }
    val negFiles = getFiles("neg", negCheck)
    if (! negFiles.isEmpty) {
      printOutline("\nTesting compiler (on files whose compilation should fail)\n")
      for (file <- negFiles) master ! NegTest(file)
    }
    val jvmFiles = getFiles("jvm", jvmCheck) ::: getFiles("run", jvmCheck) :::
                   getFiles("jvm5", jvmCheck && isJava5)
    if (! jvmFiles.isEmpty) {
      printOutline("\nTesting JVM backend\n")
      for (file <- jvmFiles) master ! JVMTest(file)
    } else {
      val runFiles = getFiles("run", runCheck)
      if (! runFiles.isEmpty) {
        printOutline("\nTesting JVM backend\n")
        for (file <- runFiles) master ! JVMTest(file)
      }
    }
    val shootFiles = getFiles("shootout", shootoutCheck)
    if (! shootFiles.isEmpty) {
      printOutline("\nTesting shootout benchmarks\n")
      for (file <- shootFiles) master! ShootoutTest(createTestFile(file, "shootout"))
    }

    master ! ("start", conservative)
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

  private def printVersion {
    println(util.Properties.versionMsg)
    exit(0)
  }

  final def printVerbose(msg: String) {
    if (verbose) {
      printOutline("debug  : ")
      println(msg)
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
      for (arg <- args) {
        arg match {
          case "--pos"          => posCheck = true
          case "--neg"          => negCheck = true
          case "--jvm"          => jvmCheck = true
          case "--run"          => runCheck = true
          case "--shootout"     => shootoutCheck = true
          case "--conservative" => conservative = true
          case "--verbose"      => verbose = true
          case "--version"      => printVersion
          case _ =>
            if (arg endsWith ".scala") {
              val file = new File(arg)
              if (file.isFile)
                testFiles += file
              else {
                println("File \"" + arg + "\" not found")
                exit(1)
              }
            } else if (out eq con) {
              val file = new File(arg)
              if (file.isFile || file.createNewFile)
                out = new PrintStream(new FileOutputStream(file))
              else {
                println("Result file \"" + arg + "\" not found")
                exit(1)
              }
            } else
              printUsage
        }
      }
      if (!(posCheck | negCheck | jvmCheck | runCheck | shootoutCheck)) {
        posCheck = true
        negCheck = true
      }
      initialization(PrintMgr.MANY)
      go
    }
  }
}
