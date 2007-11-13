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
import java.io.{File, FilenameFilter, FileOutputStream, PrintStream}

import scala.tools.nsc.Settings

import utils.PrintMgr
import utils.PrintMgr._

/**
 * @author  Adriaan Moors, Thomas Hofer
 * @version 1.0
 */
class Test(val kind: String, val file: File) {
  val dir = file.getParent
  protected def baseSettings(settings: Settings) {
    settings.classpath.value = dir
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
      if (dir endsWith "jvm5") "jvm-1.5" else "jvm-1.4"
    settings.classpath.value = System.getProperty("JVMEXTCP")
    TestRunner.printVerbose("CLASSPATH="+settings.classpath.value +"\n")
  }
}
case class ShootoutTest(override val file: File) extends Test("shootout", file) {
  override def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings.classpath.value = System.getProperty("JVMEXTCP")
  }
}

/**
 * @author  Stephane Micheloud
 * @version 1.0
 */
object TestRunner {
  private var posCheck = false
  private var negCheck = false
  private var jvmCheck = false
  private var runCheck = false
  private var shootoutCheck = false

  private var conservative = false
  private var verbose = false

  private var testDir: File = _
  private val con = new PrintStream(Console.out)
  private var out = con

  private def go {
    val master = new MasterActor(testDir, out)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name endsWith ".scala"
    }
    def getFiles(kind: String): List[File] = {
      val kindDir = "files" + File.separator + kind
      val dir = new File(testDir, kindDir)
      if (dir.isDirectory) dir.listFiles(filter).toList
      else {
        println("Directory \"" + testDir.getPath + File.separator + kindDir + "\" not found")
        Nil
      }
    }

    master.start

    if (posCheck) {
      printOutline("Testing compiler (on files whose compilation should succeed)\n")
      for (file <- getFiles("pos")) master ! PosTest(file)
    }
    if (negCheck) {
      printOutline("Testing compiler (on files whose compilation should fail)\n")
      for (file <- getFiles("neg")) master ! NegTest(file)
    }
    if (jvmCheck) {
      printOutline("Testing JVM backend\n")
      for (file <- getFiles("jvm")) master ! JVMTest(file)
      for (file <- getFiles("run")) master ! JVMTest(file)
      for (file <- getFiles("jvm5")) master ! JVMTest(file)
    } else if (runCheck) {
      printOutline("Testing JVM backend\n")
      for (file <- getFiles("run")) master ! JVMTest(file)
    }
    if (shootoutCheck) {
      printOutline("Testing shootout benchmarks\n")
      for (file <- getFiles("shootout")) master! ShootoutTest(file)
    }

    master ! ("start", conservative)
  }

  private def printUsage {
    println("Usage: TestRunner [<options>] <testdir> [<resfile>]")
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
            if (testDir eq null) {
              val dir = new File(arg)
              if (dir.isDirectory) testDir = dir
              else {
                println("Directory \"" + arg + "\" not found")
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
