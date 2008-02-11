/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream,
                PrintWriter, StringWriter, FileWriter, InputStreamReader,
                FileReader}

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
    val filesdir = file.getAbsoluteFile.getParentFile.getParentFile
    val testdir = filesdir.getParentFile
    val totalWidth = 56
    val name = {
      // 1. try with [...]/files/run/test.scala
      val testPathLen = testdir.getAbsolutePath.length
      val name = file.getAbsolutePath.substring(testPathLen)
      if (name.length <= totalWidth)
        name
      // 2. try with [...]/run/test.scala
      else {
        val filesPathLen = filesdir.getAbsolutePath.length
        file.getAbsolutePath.substring(filesPathLen)
      }
    }
    NestUI.normal("[...]"+name+List.toString(List.make(totalWidth-name.length, ' ')), printer)
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

  var log = ""

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

    val proc = Runtime.getRuntime.exec(cmd)
    val in = proc.getInputStream
    val err = proc.getErrorStream
    val writer = new FileWriter(logFile)
    val inApp = new StreamAppender(new InputStreamReader(in), writer)
    val errApp = new StreamAppender(new InputStreamReader(err), writer)
    inApp.start()
    errApp.start()
    proc.waitFor()
    inApp.join()
    errApp.join()
    writer.close()

    if (NestRunner.showLog) {
      // produce log as string in `log`
      val reader = new FileReader(logFile)
      val writer = new StringWriter
      val appender = new StreamAppender(reader, writer)
      appender.start()
      appender.join()
      reader.close()
      log = writer.toString
    }

    /*val execution = Runtime.getRuntime.exec(cmd)
    val in = execution.getInputStream
    val out = new FileOutputStream(logFile)
    var c = in.read
    while (c != -1) {
      out.write(c)
      c = in.read
    }
    in.close
    out.close*/
  }

  def compareOutput(dir: File, fileBase: String, kind: String, logFile: File): String = {
    // if check file exists, compare with log file
    val checkFile = {
      val chkFile = new File(dir, fileBase + ".check")
      if (chkFile.isFile)
        chkFile
      else
        new File(dir, fileBase + "-" + kind + ".check")
    }
    if (!checkFile.exists || !checkFile.canRead) ""
    else FileManager.compareFiles(logFile, checkFile)
  }

  def runJvmTests(kind: String, files: List[File]): (Int, Int) = {
    NestUI.verbose("testing "+files)
    val compileMgr = new CompileManager
    var errors = 0
    var success = true
    var diff = ""
    for (file <- files) {
      val fileBase: String = basename(file.getName)
      NestUI.verbose(this+" running test "+fileBase)
      val dir = file.getParentFile
      val outDir = new File(dir, fileBase + "-" + kind + ".obj")
      val logFile = new File(dir, fileBase + "-" + kind + ".log")

      // when option "--failed" is provided
      // execute test only if log file is present
      // (which means it failed before)
      if (!NestRunner.failed || (logFile.exists && logFile.canRead)) {
        val swr = new StringWriter
        val wr = new PrintWriter(swr)
        success = true
        diff = ""
        printInfoStart(file, wr)

        if (!compileMgr.shouldCompile(file, kind)) {
          NestUI.verbose("compilation of "+file+" failed\n")
          success = false
        } else {
          // -------- run test --------

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

          diff = compareOutput(dir, fileBase, kind, logFile)
          if (!diff.equals("")) {
            NestUI.verbose("output differs from log file\n")
            success = false
          }

          // delete output dir
          FileManager.deleteRecursive(outDir)

          // delete log file only if test was successful
          if (success)
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
        if (!success && NestRunner.showDiff) NestUI.normal(diff)
        if (!success && NestRunner.showLog)  NestUI.normal(log)
      }
    } // for each file
    NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
    NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
    (files.length-errors, errors)
  }

  /** Runs a list of tests.
   *
   * @param kind  The test kind (pos, neg, run, etc.)
   * @param files The list of test files
   */
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
        def negTestRun(file: File, wr: PrintWriter, log: File) = new CompileTestRun(file, wr) {
          def runTest() {
            if (!compileMgr.shouldFailCompile(file, kind, log)) {
              succeeded = false
              errors += 1
            } //TODO: else compare log file to check file
          }
        }
        for (file <- files) {
          val logFile = getLogFile(file, kind)
          val swr = new StringWriter
          val wr = new PrintWriter(swr)
          val testRun = negTestRun(file, wr, logFile)
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
