/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream,
                PrintWriter, StringWriter, FileWriter, InputStreamReader,
                FileReader, OutputStreamWriter, BufferedReader}

import java.net.URL

import scala.tools.nsc.ObjectRunner

import scala.actors.Actor
import scala.actors.Actor._

case class RunTests(kind: String, files: List[File])
case class Results(succ: Int, fail: Int, logs: List[LogFile], outdirs: List[File])

class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}

class Worker(val fileManager: FileManager) extends Actor {
  import fileManager._
  def act() {
    react {
      case RunTests(kind, files) =>
        NestUI.verbose("received "+files.length+" to test")
        val (succ, fail) = runTests(kind, files)
        sender ! Results(succ, fail, createdLogFiles, createdOutputDirs)
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

  var log = ""
  var createdLogFiles: List[LogFile] = List()
  var createdOutputDirs: List[File] = List()

  def createLogFile(dir: File, fileBase: String, kind: String): LogFile = {
    val logFile = new LogFile(dir, fileBase + "-" + kind + ".log")
    createdLogFiles = logFile :: createdLogFiles
    logFile
  }

  def createLogFile(file: File, kind: String): LogFile = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    createLogFile(dir, fileBase, kind)
  }

  def createOutputDir(dir: File, fileBase: String, kind: String): File = {
    val outDir = new File(dir, fileBase + "-" + kind + ".obj")
    createdOutputDirs = outDir :: createdOutputDirs
    outDir
  }

  /* Note: not yet used/tested. */
  def execTestObjectRunner(file: File, outDir: File, logFile: File) {
    val classpath: List[URL] =
      outDir.toURL ::
      List(file.getParentFile.toURL) :::
      (List.fromString(CLASSPATH, File.pathSeparatorChar) map { x =>
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

  def execTest(outDir: File, logFile: File) {
    val cmd =
      JAVACMD+
      " -classpath "+outDir+File.pathSeparatorChar+CLASSPATH+
      " -Djava.library.path="+logFile.getParentFile.getAbsolutePath+
      " -Dscalatest.output="+outDir.getAbsolutePath+
      " -Dscalatest.lib="+LATEST_LIB+
      " -Djavacmd="+JAVACMD+
      " scala.tools.nsc.MainGenericRunner"+
      " Test jvm"
    //NestUI.verbose(cmd)

    val proc = Runtime.getRuntime.exec(cmd)
    val in = proc.getInputStream
    val err = proc.getErrorStream
    val writer = new PrintWriter(new FileWriter(logFile), true)
    val inApp = new StreamAppender(new BufferedReader(new InputStreamReader(in)),
                                   writer)
    val errApp = new StreamAppender(new BufferedReader(new InputStreamReader(err)),
                                    writer)
    val async = new Thread(errApp)
    async.start()
    inApp.run()
    async.join()

    //proc.waitFor()

    writer.close()

    /*val execution = Runtime.getRuntime.exec(cmd)
    val in = execution.getInputStream
    val err = execution.getErrorStream
    val out = new FileOutputStream(logFile)
    var c1 = in.read
    var c2 = err.read
    while ((c1 != -1) || (c2 != -1)) {
      if (c1 != -1) {
        out.write(c1)
        c1 = in.read
      } else {
        out.write(c2)
        c2 = err.read
      }
    }
    in.close
    err.close
    out.close*/

    if (fileManager.showLog) {
      // produce log as string in `log`
      val reader = new BufferedReader(new FileReader(logFile))
      val swriter = new StringWriter
      val pwriter = new PrintWriter(swriter, true)
      val appender = new StreamAppender(reader, writer)
      appender.run()
      log = swriter.toString
    }
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
    else fileManager.compareFiles(logFile, checkFile)
  }

  def runJvmTests(kind: String, files: List[File]): (Int, Int) = {
    NestUI.verbose("testing "+files)
    val compileMgr = new CompileManager(fileManager)
    var errors = 0
    var success = true
    var diff = ""
    for (file <- files) {
      val fileBase: String = basename(file.getName)
      NestUI.verbose(this+" running test "+fileBase)
      val dir = file.getParentFile
      val outDir = createOutputDir(dir, fileBase, kind)
      val logFile = createLogFile(dir, fileBase, kind)

      // when option "--failed" is provided
      // execute test only if log file is present
      // (which means it failed before)
      if (!fileManager.failed || (logFile.exists && logFile.canRead)) {
        val swr = new StringWriter
        val wr = new PrintWriter(swr)
        success = true
        diff = ""
        printInfoStart(file, wr)

        try { // *catch-all*

          if (!compileMgr.shouldCompile(file, kind)) {
            NestUI.verbose("compilation of "+file+" failed\n")
            success = false
          } else {
            // -------- run test --------

            //TODO: detect whether we have to use Runtime.exec
            val useRuntime = true

            if (useRuntime)
              execTest(outDir, logFile)
            else
              execTestObjectRunner(file, outDir, logFile)
            NestUI.verbose(this+" finished running "+fileBase)

            diff = compareOutput(dir, fileBase, kind, logFile)
            if (!diff.equals("")) {
              NestUI.verbose("output differs from log file\n")
              success = false
            }

            // delete log file only if test was successful
            if (success)
              logFile.toDelete = true
          } // successful compile
        } catch { // *catch-all*
          case e: Exception =>
            NestUI.verbose("caught "+e)
            success = false
        }

        if (!success) {
          errors += 1
          NestUI.verbose("incremented errors: "+errors)
        }
        printInfoEnd(success, wr)
        wr.flush()
        swr.flush()
        NestUI.normal(swr.toString)
        if (!success && fileManager.showDiff) NestUI.normal(diff)
        if (!success && fileManager.showLog)  NestUI.normal(log)
      }
    } // for each file
    NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
    NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
    (files.length-errors, errors)
  }

  abstract class TestRun(file: File) {
    def beforeRun(): Unit
    def runTest(): Unit
    def afterRun(): Unit
    def doAll() {
      beforeRun(); runTest(); afterRun()
    }
  }

  /** Runs a list of tests.
   *
   * @param kind  The test kind (pos, neg, run, etc.)
   * @param files The list of test files
   */
  def runTests(kind: String, files: List[File]): (Int, Int) = {
    val compileMgr = new CompileManager(fileManager)
    var errors = 0
    var succeeded = true
    var diff = ""
    var log = ""

    abstract class CompileTestRun(file: File, wr: PrintWriter) extends TestRun(file) {
      def beforeRun() {
        succeeded = true;
        diff = ""
        log = ""
        printInfoStart(file, wr)
      }
      def afterRun() { printInfoEnd(succeeded, wr) }
    }

    kind match {
      case "pos" => {
        def posTestRun(file: File, wr: PrintWriter) = new CompileTestRun(file, wr) {
          def runTest() {
            try {
              val fileBase: String = basename(file.getName)
              NestUI.verbose(this+" running test "+fileBase)
              val dir = file.getParentFile
              val outDir = createOutputDir(dir, fileBase, kind)

              if (!compileMgr.shouldCompile(file, kind)) {
                succeeded = false
                errors += 1
              }
            } catch {
              case e: Exception =>
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
        for (file <- files) {
          // when option "--failed" is provided
          // execute test only if log file is present
          // (which means it failed before)
          val logFile = createLogFile(file, kind)
          if (!fileManager.failed || (logFile.exists && logFile.canRead)) {
            val swr = new StringWriter
            val wr = new PrintWriter(swr)
            succeeded = true; diff = ""; log = ""
            printInfoStart(file, wr)

            try {
              if (!compileMgr.shouldFailCompile(file, kind, logFile)) {
                succeeded = false
                errors += 1
              } else { // compare log file to check file
                val fileBase: String = basename(file.getName)
                val dir = file.getParentFile
                val outDir = createOutputDir(dir, fileBase, kind)

                NestUI.verbose("comparing output with check file...")
                diff = compareOutput(dir, fileBase, kind, logFile)
                if (!diff.equals("")) {
                  NestUI.verbose("output differs from log file\n")
                  succeeded = false
                  errors += 1
                }

                // delete log file only if test was successful
                if (succeeded)
                  logFile.toDelete = true
              }
            } catch {
              case e: Exception =>
                NestUI.verbose("caught "+e)
                succeeded = false
                errors += 1
            }

            printInfoEnd(succeeded, wr)
            wr.flush()
            swr.flush()
            NestUI.normal(swr.toString)

            if (!succeeded && fileManager.showDiff) NestUI.normal(diff)
            if (!succeeded && fileManager.showLog) {
              // output log file
              val logReader = new BufferedReader(new FileReader(logFile))
              val logWriter = new PrintWriter(new StringWriter, true)
              val logAppender = new StreamAppender(logReader, logWriter)
              logAppender.run()
              val log = logWriter.toString
              NestUI.normal(log)
            }
          }
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
      case "res" => {
        for (file <- files) {
          val swr = new StringWriter
          val wr = new PrintWriter(swr)
          succeeded = true; diff = ""; log = ""
          printInfoStart(file, wr)

          val fileBase: String = basename(file.getName)
          NestUI.verbose(this+" running test "+fileBase)
          val dir = file.getParentFile
          val outDir = createOutputDir(dir, fileBase, kind)
          if (!outDir.exists) outDir.mkdir()
          val logFile = createLogFile(dir, fileBase, kind)
          val resFile = new File(dir, fileBase + ".res")

          // run scalac in resident mode:
          // $SCALAC -d "$os_dstbase".obj -Xresident -sourcepath . "$@"
          val cmd =
            JAVACMD+
            " -classpath "+outDir+File.pathSeparator+CLASSPATH+
            " -Djavacmd="+JAVACMD+
            " scala.tools.nsc.Main"+
            " -d "+outDir.getCanonicalFile.getAbsolutePath+
            " -Xresident"+
            " -sourcepath "+logFile.getParentFile.getCanonicalFile.getAbsolutePath
          NestUI.verbose(cmd)

          val proc = Runtime.getRuntime.exec(cmd, null, dir)

          val in = proc.getInputStream
          val err = proc.getErrorStream
          val out = proc.getOutputStream

          val writer = new PrintWriter(new FileWriter(logFile), true)
          val reader = new BufferedReader(new FileReader(resFile))
          val errWriter = new PrintWriter(new StringWriter, true)

          val inApp = new StreamAppender(new BufferedReader(new InputStreamReader(in)),
                                         writer)
          val errApp = new StreamAppender(new BufferedReader(new InputStreamReader(err)),
                                          errWriter)
          val outApp = new StreamAppender(reader, new PrintWriter(new OutputStreamWriter(out), true))

          val inThr = new Thread(inApp)
          val errThr = new Thread(errApp)
          inThr.start()
          errThr.start()
          outApp.run()

          val exitCode = proc.waitFor()
          NestUI.verbose("finished with exit code "+exitCode)

          inThr.join()
          errThr.join()

          //writer.close()
          //reader.close()

          // compare log file with check file
          diff = compareOutput(dir, fileBase, kind, logFile)
          if (!diff.equals("")) {
            NestUI.verbose("output differs from log file\n")
            succeeded = false
            errors += 1
          }

          if (!succeeded && fileManager.showDiff) NestUI.normal(diff)
          if (!succeeded && fileManager.showLog) {
            // output log file
            val logReader = new BufferedReader(new FileReader(logFile))
            val logWriter = new PrintWriter(new StringWriter, true)
            val logAppender = new StreamAppender(logReader, logWriter)
            logAppender.run()
            //TODO: close logReader
            val log = logWriter.toString
            NestUI.normal(log)
          }

          // delete log file only if test was successful
          if (succeeded)
            logFile.toDelete = true

          printInfoEnd(succeeded, wr)
          wr.flush()
          swr.flush()
          NestUI.normal(swr.toString)
        }
        NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
        NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
        (files.length-errors, errors)
      }
    }
  }
}
