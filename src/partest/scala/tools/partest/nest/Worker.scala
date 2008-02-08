/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream,
                PrintWriter, StringWriter}

import java.net.URL

import scala.tools.nsc.ObjectRunner

import scala.actors.Actor
import scala.actors.Actor._

import FileManager._

case class RunTests(kind: String, files: List[File])
case class Results(succ: Int, fail: Int)

class Worker extends Actor {
  def act() {
    react {
      case RunTests(kind, files) =>
        NestUI.verbose("received "+files.length+" to test")
        val (succ, fail) = runTests(kind, files)
        sender ! Results(succ, fail)
    }
  }

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def printInfoStart(file: File, printer: PrintWriter) {
    NestUI.outline("testing: ", printer)
    val dir = file.getParentFile
    val dirpath = dir.getAbsolutePath
    val name = file.getAbsolutePath.substring(dirpath.length)
    val WIDTH = 56
    NestUI.normal("[...]"+name+List.toString(List.make(WIDTH-name.length, ' ')), printer)
  }

  def printInfoEnd(success: boolean, printer: PrintWriter) {
    NestUI.normal("[", printer)
    if (success) NestUI.success("  OK  ", printer)
    else NestUI.failure("FAILED", printer)
    NestUI.normal("]\n", printer)
  }

  abstract class TestRun(file: File) {
    def beforeRun(): Unit
    def runTest(): Unit
    def afterRun(): Unit
    def doAll() {
      beforeRun(); runTest(); afterRun()
    }
  }

  def execTest(outDir: File, logFile: File) {
    val cmd =
      JAVACMD+
      " -classpath "+outDir+PATH_SEP+CLASSPATH+PATH_SEP+EXT_CLASSPATH+
      " -Djava.library.path="+logFile.getParentFile.getAbsolutePath+
      " -Dscalatest.output="+outDir.getAbsolutePath+
      " -Dscalatest.lib="+LATEST_LIB+
      " -Djavacmd="+JAVACMD+
      " scala.tools.nsc.MainGenericRunner"+
      " Test jvm"
    NestUI.verbose(cmd)
    val execution = Runtime.getRuntime.exec(cmd)
    //TODO: use buffered I/O
    val in = execution.getInputStream
    val out = new FileOutputStream(logFile)
    var c = in.read
    while (c != -1) {
      out.write(c)
      c = in.read
    }
    in.close
    out.close
  }

  def compareOutput(dir: File, fileBase: String, kind: String, logFile: File): Boolean = {
    // if check file exists, compare with log file
    val checkFile = {
      val chkFile = new File(dir, fileBase + ".check")
      if (chkFile.isFile)
        chkFile
      else
        new File(dir, fileBase + "-" + kind + ".check")
    }
    if (!checkFile.exists || !checkFile.canRead)
      true
    else {
      var success, equalNow = true
      val bufferSize = 1024
      val originBuffer, destBuffer = new Array[Byte](bufferSize)
      val originStream = new FileInputStream(logFile)
      val destStream = new FileInputStream(checkFile)

      var originSize = originStream.read(originBuffer)
      while (originSize >= 0) {
        if (originSize == destStream.read(destBuffer)) {
          for (idx <- 0 until originSize)
            equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
          if (!equalNow) {
            success = false
            NestUI.verbose("Diff1: diffs found")
          }
        } else {
          success = false
          NestUI.verbose("Diff1: diffs found")
        }
        originSize = originStream.read(originBuffer)
      }
      if (destStream.read(destBuffer) >= 0)
        success = false
      success
    }
  }

  def runJvmTests(kind: String, files: List[File]): (Int, Int) = {
    val compileMgr = new CompileManager
    var errors = 0
    var success = true
    for (file <- files) {
      val swr = new StringWriter
      val wr = new PrintWriter(swr)
      success = true
      printInfoStart(file, wr)

      if (!compileMgr.shouldCompile(file, kind)) {
        NestUI.verbose("compilation of "+file+" failed\n")
        success = false
      } else {
        // -------- run test --------
        val fileBase: String = basename(file.getName)
        NestUI.verbose(this+" running test "+fileBase)
        val dir = file.getParentFile
        val dirpath = dir.getAbsolutePath
        val outDir = new File(dir, fileBase + "-" + kind + ".obj")
        val logFile = new File(dir, fileBase + "-" + kind + ".log")
        //TODO: detect whether we have to use Runtime.exec
        val useRuntime = true

        if (useRuntime) {
          execTest(outDir, logFile)
        } else {
          val classpath: List[URL] =
            outDir.toURL ::
            List(file.getParentFile.toURL) :::
            (List.fromString(CLASSPATH, PATH_SEP) map { x =>
              (new File(x)).toURL }) :::
            (List.fromString(EXT_CLASSPATH, PATH_SEP) map { x =>
              (new File(x)).toURL })
          try {
            NestUI.verbose("classpath: "+classpath)
            val out = new FileOutputStream(logFile, true)
            Console.withOut(new PrintStream(out)) {
              ObjectRunner.run(classpath, "Test", List("jvm"))
            }
            out.flush
            out.close
          } catch {
            case e: Exception =>
              NestUI.verbose(e+" ("+file.getPath+")")
          }
        }
        NestUI.verbose(this+" finished running "+fileBase)

        if (!compareOutput(dir, fileBase, kind, logFile)) {
          NestUI.verbose("output differs from log file\n")
          success = false
        }

        // delete log file and output dir
        FileManager.deleteRecursive(outDir)
        FileManager.deleteRecursive(logFile)
      } // successful compile
      if (!success) {
        errors += 1
        NestUI.verbose("incremented errors: "+errors)
      }
      printInfoEnd(success, wr)
      wr.flush()
      swr.flush()
      NestUI.normal(swr.toString)
    } // for each file
    NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
    NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
    (files.length-errors, errors)
  }

  def runTests(kind: String, files: List[File]): (Int, Int) = {
    val compileMgr = new CompileManager
    var errors = 0
    var succeeded = true

    abstract class CompileTestRun(file: File, wr: PrintWriter) extends TestRun(file) {
      def beforeRun() { succeeded = true; printInfoStart(file, wr) }
      def afterRun() { printInfoEnd(succeeded, wr) }
    }

    kind match {
      case "pos" => {
        def posTestRun(file: File, wr: PrintWriter) = new CompileTestRun(file, wr) {
          def runTest() {
            if (!compileMgr.shouldCompile(file, kind)) {
              succeeded = false
              errors += 1
            }
          }
        }
        for (file <- files) {
          val swr = new StringWriter
          val wr = new PrintWriter(swr)
          val testRun = posTestRun(file, wr)
          testRun.doAll()
          wr.flush()
          swr.flush()
          NestUI.normal(swr.toString)
        }
        NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
        NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
        (files.length-errors, errors)
      }
      case "neg" => {
        def negTestRun(file: File, wr: PrintWriter) = new CompileTestRun(file, wr) {
          def runTest() {
            if (!compileMgr.shouldFailCompile(file, kind)) {
              succeeded = false
              errors += 1
            }
          }
        }
        for (file <- files) {
          val swr = new StringWriter
          val wr = new PrintWriter(swr)
          val testRun = negTestRun(file, wr)
          testRun.doAll()
          wr.flush()
          swr.flush()
          NestUI.normal(swr.toString)
        }
        NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
        NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
        (files.length-errors, errors)
      }
      case "run" => {
        runJvmTests(kind, files)
      }
      case "jvm" => {
        runJvmTests(kind, files)
      }
      case "jvm5" => {
        runJvmTests(kind, files)
      }
    }
  }
}
