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
  import scala.tools.nsc.{Settings, CompilerCommand, Global}
  import scala.tools.nsc.reporters.ConsoleReporter
  import scala.tools.nsc.util.FakePos

  var reporter: ConsoleReporter = _

  def error(msg: String) {
    reporter.error(FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

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

  def execTest(outDir: File, logFile: File, fileBase: String) {
    // check whether there is a ".javaopts" file
    val argsFile = new File(logFile.getParentFile, fileBase+".javaopts")
    val argString = if (argsFile.exists) {
      NestUI.verbose("argsFile: "+argsFile)
      val fileReader = new FileReader(argsFile)
      val reader = new BufferedReader(fileReader)
      val options = reader.readLine()
      reader.close()
      options
    } else ""
    NestUI.verbose("JAVA_OPTS: "+argString)

    val cmd =
      JAVACMD+
      " "+argString+
      " -classpath "+outDir+File.pathSeparatorChar+CLASSPATH+
      " -Djava.library.path="+logFile.getParentFile.getAbsolutePath+
      " -Dscalatest.output="+outDir.getAbsolutePath+
      " -Dscalatest.lib="+LATEST_LIB+
      " -Dscalatest.cwd="+outDir.getParent+
      " -Djavacmd="+JAVACMD+
      " scala.tools.nsc.MainGenericRunner"+
      " Test jvm"
    NestUI.verbose(cmd)

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
    writer.close()

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

    def runInContext(file: File, kind: String, script: (File, File) => Unit) {
      // when option "--failed" is provided
      // execute test only if log file is present
      // (which means it failed before)
      val logFile = createLogFile(file, kind)
      if (!fileManager.failed || (logFile.exists && logFile.canRead)) {
        val swr = new StringWriter
        val wr = new PrintWriter(swr)
        succeeded = true
        diff = ""
        log = ""
        printInfoStart(file, wr)

        val fileBase: String = basename(file.getName)
        NestUI.verbose(this+" running test "+fileBase)
        val dir = file.getParentFile
        val outDir = createOutputDir(dir, fileBase, kind)

        // run test-specific code
        try {
          script(logFile, outDir)
        } catch {
          case e: Exception =>
            e.printStackTrace
            succeeded = false
        }

        if (!succeeded) {
          errors += 1
          NestUI.verbose("incremented errors: "+errors)
        } else {
          // delete log file only if test was successful
          logFile.toDelete = true
        }

        printInfoEnd(succeeded, wr)
        wr.flush()
        swr.flush() //TODO: needed?
        NestUI.normal(swr.toString)
        if (!succeeded && fileManager.showDiff && diff != "")
          NestUI.normal(diff)
        else if (!succeeded && fileManager.showLog)
          showLog(logFile)
      }
    }

    def runJvmTest(file: File, kind: String) {
      runInContext(file, kind, (logFile: File, outDir: File) => {
        if (!compileMgr.shouldCompile(file, kind, logFile)) {
          NestUI.verbose("compilation of "+file+" failed\n")
          succeeded = false
        } else { // run test
          val fileBase = basename(file.getName)
          val dir      = file.getParentFile

          //TODO: detect whether we have to use Runtime.exec
          val useRuntime = true

          if (useRuntime)
            execTest(outDir, logFile, fileBase)
          else
            execTestObjectRunner(file, outDir, logFile)
          NestUI.verbose(this+" finished running "+fileBase)

          diff = compareOutput(dir, fileBase, kind, logFile)
          if (!diff.equals("")) {
            NestUI.verbose("output differs from log file\n")
            succeeded = false
          }
        }
      })
    }

    kind match {
      case "pos" =>
        for (file <- files) {
          runInContext(file, kind, (logFile: File, outDir: File) => {
            if (!compileMgr.shouldCompile(file, kind, logFile)) {
              NestUI.verbose("compilation of "+file+" failed\n")
              succeeded = false
            }
          })
        }
      case "neg" =>
        for (file <- files) {
          runInContext(file, kind, (logFile: File, outDir: File) => {
            if (!compileMgr.shouldFailCompile(file, kind, logFile)) {
              succeeded = false
            } else { // compare log file to check file
              val fileBase = basename(file.getName)
              val dir      = file.getParentFile
              diff = compareOutput(dir, fileBase, kind, logFile)
              if (!diff.equals("")) {
                NestUI.verbose("output differs from log file\n")
                succeeded = false
              }
            }
          })
        }
      case "run" =>
        for (file <- files) {
          runJvmTest(file, kind)
        }
      case "jvm" =>
        for (file <- files) {
          runJvmTest(file, kind)
        }
      case "jvm5" =>
        for (file <- files) {
          runJvmTest(file, kind)
        }
      case "res" => {
        for (file <- files) {
          // when option "--failed" is provided
          // execute test only if log file is present
          // (which means it failed before)

          //val (logFileOut, logFileErr) = createLogFiles(file, kind)
          val logFile = createLogFile(file, kind)
          if (!fileManager.failed || (logFile.exists && logFile.canRead)) {
            val swr = new StringWriter
            val wr = new PrintWriter(swr)
            succeeded = true; diff = ""; log = ""
            printInfoStart(file, wr)

            val fileBase: String = basename(file.getName)
            NestUI.verbose(this+" running test "+fileBase)
            val dir = file.getParentFile
            val outDir = createOutputDir(dir, fileBase, kind)
            if (!outDir.exists) outDir.mkdir()
            val resFile = new File(dir, fileBase + ".res")
            NestUI.verbose("outDir:  "+outDir)
            NestUI.verbose("logFile: "+logFile)
            //NestUI.verbose("logFileErr: "+logFileErr)
            NestUI.verbose("resFile: "+resFile)

            // run compiler in resident mode
            // $SCALAC -d "$os_dstbase".obj -Xresident -sourcepath . "$@"

            val sourcedir  = logFile.getParentFile.getCanonicalFile
            val sourcepath = sourcedir.getAbsolutePath+"/"
            NestUI.verbose("sourcepath: "+sourcepath)

            val argString =
              "-d "+outDir.getCanonicalFile.getAbsolutePath+
              " -Xresident"+
              " -sourcepath "+sourcepath
            val argList = List.fromString(argString, ' ')

            // configure input/output files
            val logOut    = new FileOutputStream(logFile)
            val logWriter = new PrintStream(logOut)
            val resReader = new BufferedReader(new FileReader(resFile))
            val logConsoleWriter = new PrintWriter(new OutputStreamWriter(logOut))

            // create compiler
            val settings = new Settings(error)
            settings.sourcepath.value = sourcepath
            settings.classpath.value = fileManager.CLASSPATH
            reporter = new ConsoleReporter(settings, Console.in, logConsoleWriter)
            val command = new CompilerCommand(argList, settings, error, false)
            object compiler extends Global(command.settings, reporter)

            // simulate resident compiler loop
            val prompt = "\nnsc> "

            val resCompile = (line: String) => {
              NestUI.verbose("compiling "+line)
              val cmdArgs = List.fromString(line, ' ') map { fs => new File(dir, fs).getAbsolutePath }
              val sett = new Settings(error)
              sett.sourcepath.value = sourcepath
              val command = new CompilerCommand(cmdArgs, sett, error, true)
              (new compiler.Run) compile command.files
            }

            def loop(action: (String) => Unit) {
              logWriter.print(prompt)
              val line = resReader.readLine()
              if ((line ne null) && line.length() > 0) {
                action(line)
                loop(action)
              }
            }
            val oldStdOut = System.out
            val oldStdErr = System.err
            System.setOut(logWriter)
            System.setErr(logWriter)
            loop(resCompile)
            resReader.close()
            logWriter.flush()
            logWriter.close()

            System.setOut(oldStdOut)
            System.setErr(oldStdErr)

            val tempLogFile = new File(dir, ".temp.log")
            val logFileReader = new BufferedReader(new FileReader(logFile))
            val tempLogFilePrinter = new PrintWriter(tempLogFile)
            val appender =
              new StreamAppender(logFileReader, tempLogFilePrinter)
            appender.runAndMap({ s => s.replaceAll(dir.getAbsolutePath.replace(File.separatorChar,'/')+File.separator, "") })
            logFileReader.close()
            tempLogFilePrinter.close()

            val tempLogFileReader = new BufferedReader(new FileReader(tempLogFile))
            val logFilePrinter = new PrintWriter(logFile)
            (new StreamAppender(tempLogFileReader, logFilePrinter)).run
            tempLogFileReader.close()
            logFilePrinter.close()

            tempLogFile.delete()

            diff = compareOutput(dir, fileBase, kind, logFile)
            if (!diff.equals("")) {
              NestUI.verbose("output differs from log file\n")
              succeeded = false
            }

            // delete log file only if test was successful
            if (succeeded)
              logFile.toDelete = true

            if (!succeeded) {
              errors += 1
              NestUI.verbose("incremented errors: "+errors)
            }
            printInfoEnd(succeeded, wr)
            wr.flush()
            swr.flush()
            NestUI.normal(swr.toString)

            if (!succeeded && fileManager.showDiff) NestUI.normal(diff)
            if (!succeeded && fileManager.showLog) showLog(logFile)
          }
        }
      }
      case "shootout" => {
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

            val fileBase: String = basename(file.getName)
            NestUI.verbose(this+" running test "+fileBase)
            val dir = file.getParentFile
            val outDir = createOutputDir(dir, fileBase, kind)
            if (!outDir.exists) outDir.mkdir()

            // 2. define file {outDir}/test.scala that contains code to compile/run
            val testFile = new File(outDir, "test.scala")
            NestUI.verbose("outDir:   "+outDir)
            NestUI.verbose("logFile:  "+logFile)
            NestUI.verbose("testFile: "+testFile)

            // 3. cat {test}.scala.runner {test}.scala > testFile
            val runnerFile = new File(dir, fileBase+".scala.runner")
            val bodyFile   = new File(dir, fileBase+".scala")
            val appender = StreamAppender.concat(new FileInputStream(runnerFile),
                                                 new FileInputStream(bodyFile),
                                                 new FileOutputStream(testFile))
            appender.run()

            try { // *catch-all*
              // 4. compile testFile
              if (!compileMgr.shouldCompile(testFile, kind, logFile)) {
                NestUI.verbose("compilation of "+file+" failed\n")
                succeeded = false
              } else {
                NestUI.verbose("compilation of "+testFile+"succeeded")
                // -------- run test --------

                //TODO: detect whether we have to use Runtime.exec
                val useRuntime = true

                if (useRuntime)
                  execTest(outDir, logFile, fileBase)
                else
                  execTestObjectRunner(file, outDir, logFile)
                NestUI.verbose(this+" finished running "+fileBase)
              } // successful compile
            } catch { // *catch-all*
              case e: Exception =>
                NestUI.verbose("caught "+e)
                succeeded = false
            }

            diff = compareOutput(dir, fileBase, kind, logFile)
            if (!diff.equals("")) {
              NestUI.verbose("output differs from log file\n")
              succeeded = false
            }

            // delete log file only if test was successful
            if (succeeded)
              logFile.toDelete = true

            if (!succeeded) {
              errors += 1
              NestUI.verbose("incremented errors: "+errors)
            }
            printInfoEnd(succeeded, wr)
            wr.flush()
            swr.flush()
            NestUI.normal(swr.toString)

            if (!succeeded && fileManager.showDiff) NestUI.normal(diff)
            if (!succeeded && fileManager.showLog) showLog(logFile)
          }
        }
      }
      case "script" => {
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

            val fileBase: String = basename(file.getName)
            NestUI.verbose(this+" running test "+fileBase)

            // check whether there is an args file
            val argsFile = new File(file.getParentFile, fileBase+".args")
            NestUI.verbose("argsFile: "+argsFile)
            val argString = if (argsFile.exists) {
              val swriter = new StringWriter
              val app = StreamAppender(new BufferedReader(new FileReader(argsFile)),
                                       swriter)
              app.run()
              " "+swriter.toString
            } else ""

            try {
              val proc = Runtime.getRuntime.exec(file.getAbsolutePath+argString)
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

              writer.close()

              diff = compareOutput(file.getParentFile, fileBase, kind, logFile)
              if (!diff.equals("")) {
                NestUI.verbose("output differs from log file\n")
                succeeded = false
              }
            } catch { // *catch-all*
              case e: Exception =>
                NestUI.verbose("caught "+e)
                succeeded = false
            }

            // delete log file only if test was successful
            if (succeeded)
              logFile.toDelete = true

            if (!succeeded) {
              errors += 1
              NestUI.verbose("incremented errors: "+errors)
            }
            printInfoEnd(succeeded, wr)
            wr.flush()
            swr.flush()
            NestUI.normal(swr.toString)

            if (!succeeded && fileManager.showDiff) NestUI.normal(diff)
            if (!succeeded && fileManager.showLog) showLog(logFile)
          }
        }
      }
    }
    NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
    NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
    (files.length-errors, errors)
  }

  def showLog(logFile: File) {
    val logReader = new BufferedReader(new FileReader(logFile))
    val strWriter = new StringWriter
    val logWriter = new PrintWriter(strWriter, true)
    val logAppender = new StreamAppender(logReader, logWriter)
    logAppender.run()
    logReader.close()
    val log = strWriter.toString
    NestUI.normal(log)
  }
}
