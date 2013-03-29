/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io._
import java.net.URL
import java.util.{ Timer, TimerTask }

import scala.tools.nsc.Properties.{ jdkHome, javaHome, propOrElse }
import scala.util.Properties.{ envOrElse, isWin }
import scala.tools.nsc.{ Settings, CompilerCommand, Global }
import scala.tools.nsc.io.{ AbstractFile, PlainFile, Path, Directory, File => SFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ ClassPath, FakePos, ScalaClassLoader, stackTraceString }
import ClassPath.{ join, split }
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.collection.{ mutable, immutable }
import scala.tools.nsc.interactive.{ BuildManager, RefinedBuildManager }
import scala.sys.process._
import java.util.concurrent.{ Executors, TimeUnit, TimeoutException }
import PartestDefaults.{ javaCmd, javacCmd }

class LogContext(val file: File, val writers: Option[(StringWriter, PrintWriter)])

object LogContext {
  def apply(file: File, swr: StringWriter, wr: PrintWriter): LogContext = {
    require (file != null)
    new LogContext(file, Some((swr, wr)))
  }
  def apply(file: File): LogContext = new LogContext(file, None)
}

object Output {
  object outRedirect extends Redirecter(out)
  object errRedirect extends Redirecter(err)

  System.setOut(outRedirect)
  System.setErr(errRedirect)

  import scala.util.DynamicVariable
  private def out = java.lang.System.out
  private def err = java.lang.System.err
  private val redirVar = new DynamicVariable[Option[PrintStream]](None)

  class Redirecter(stream: PrintStream) extends PrintStream(new OutputStream {
    def write(b: Int) = withStream(_ write b)

    private def withStream(f: PrintStream => Unit) = f(redirVar.value getOrElse stream)

    override def write(b: Array[Byte]) = withStream(_ write b)
    override def write(b: Array[Byte], off: Int, len: Int) = withStream(_.write(b, off, len))
    override def flush = withStream(_.flush)
    override def close = withStream(_.close)
  })

  // this supports thread-safe nested output redirects
  def withRedirected[T](newstream: PrintStream)(func: => T): T = {
    // note down old redirect destination
    // this may be None in which case outRedirect and errRedirect print to stdout and stderr
    val saved = redirVar.value
    // set new redirecter
    // this one will redirect both out and err to newstream
    redirVar.value = Some(newstream)

    try func
    finally {
      newstream.flush()
      redirVar.value = saved
    }
  }
}

class RunnerManager(kind: String, val fileManager: FileManager, params: TestRunParams) {
  import fileManager._

  val compileMgr = new CompileManager(fileManager)
  fileManager.CLASSPATH += File.pathSeparator + PathSettings.scalaCheck
  fileManager.CLASSPATH += File.pathSeparator + PathSettings.diffUtils // needed to put diffutils on test/partest's classpath

  private def compareFiles(f1: File, f2: File): String =
    try fileManager.compareFiles(f1, f2)
    catch { case t: Exception => t.toString }

  /** This does something about absolute paths and file separator
   *  chars before diffing.
   */
  private def replaceSlashes(dir: File, s: String): String = {
    val base = (dir.getAbsolutePath + File.separator).replace('\\', '/')
    var regex = """\Q%s\E""" format base
    if (isWin) regex = "(?i)" + regex
    s.replace('\\', '/').replaceAll(regex, "")
  }

  private def workerError(msg: String): Unit = System.err.println("Error: " + msg)

  private def printInfoStart(file: File, printer: PrintWriter) {
    NestUI.outline("testing: ", printer)
    val filesdir = file.getAbsoluteFile.getParentFile.getParentFile
    val testdir = filesdir.getParentFile
    val totalWidth = 56
    val name = {
      // 1. try with [...]/files/run/test.scala
      val name = file.getAbsolutePath drop testdir.getAbsolutePath.length
      if (name.length <= totalWidth) name
      // 2. try with [...]/run/test.scala
      else file.getAbsolutePath drop filesdir.getAbsolutePath.length
    }
    NestUI.normal("[...]%s%s".format(name, " " * (totalWidth - name.length)), printer)
  }

  private def printInfoEnd(success: Boolean, printer: PrintWriter) {
    NestUI.normal("[", printer)
    if (success) NestUI.success("  OK  ", printer)
    else NestUI.failure("FAILED", printer)
    NestUI.normal("]\n", printer)
  }

  private def printInfoTimeout(printer: PrintWriter) {
    NestUI.normal("[", printer)
    NestUI.failure("TIMOUT", printer)
    NestUI.normal("]\n", printer)
  }

  private def javac(outDir: File, files: List[File], output: File): CompilationOutcome = {
    // compile using command-line javac compiler
    val args = Seq(
      javacCmd,
      "-d",
      outDir.getAbsolutePath,
      "-classpath",
      join(outDir.toString, CLASSPATH)
    ) ++ files.map("" + _)

    try if (runCommand(args, output)) CompileSuccess else CompileFailed
    catch exHandler(output, "javac command failed:\n" + args.map("  " + _ + "\n").mkString + "\n", CompilerCrashed)
  }

  /** Runs command redirecting standard out and error out to output file.
   *  Overloaded to accept a sequence of arguments.
   */
  private def runCommand(args: Seq[String], outFile: File): Boolean = {
    NestUI.verbose("running command:\n"+args.map("  " + _ + "\n").mkString)
    runCommandImpl(Process(args), outFile)
  }

  /** Runs command redirecting standard out and error out to output file.
   *  Overloaded to accept a single string = concatenated command + arguments.
   */
  private def runCommand(command: String, outFile: File): Boolean = {
    NestUI.verbose("running command:"+command)
    runCommandImpl(Process(command), outFile)
  }

  private def runCommandImpl(process: => ProcessBuilder, outFile: File): Boolean = {
    val exitCode = (process #> outFile !)
    // normalize line endings
    // System.getProperty("line.separator") should be "\n" here
    // so reading a file and writing it back should convert all CRLFs to LFs
    SFile(outFile).printlnAll(SFile(outFile).lines.toList: _*)
    exitCode == 0
  }

  @inline private def isJava(f: File) = SFile(f) hasExtension "java"
  @inline private def isScala(f: File) = SFile(f) hasExtension "scala"
  @inline private def isJavaOrScala(f: File) = isJava(f) || isScala(f)

  private def outputLogFile(logFile: File) {
    val lines = SFile(logFile).lines
    if (lines.nonEmpty) {
      NestUI.normal("Log file '" + logFile + "': \n")
      lines foreach (x => NestUI.normal(x + "\n"))
    }
  }
  private def logStackTrace(logFile: File, t: Throwable, msg: String): Boolean = {
    SFile(logFile).writeAll(msg, stackTraceString(t))
    outputLogFile(logFile) // if running the test threw an exception, output log file
    false
  }

  private def exHandler[T](logFile: File, msg: String, value: T): PartialFunction[Throwable, T] = {
    case e: Exception => logStackTrace(logFile, e, msg) ; value
  }

  class Runner(testFile: File) {
    var testDiff: String = ""
    var passed: Option[Boolean] = None

    val fileBase = basename(testFile.getName)
    val logFile  = fileManager.getLogFile(testFile, kind)
    val parent   = testFile.getParentFile
    val outDir   = new File(parent, "%s-%s.obj".format(fileBase, kind))
    def toDelete = if (isPartestDebug) Nil else List(
      if (passed exists (x => x)) Some(logFile) else None,
      if (outDir.isDirectory) Some(outDir) else None
    ).flatten

    private def createOutputDir(): File = {
      outDir.mkdirs()
      outDir
    }

    private def execTest(outDir: File, logFile: File, classpathPrefix: String = "", javaOpts: String = ""): Boolean = {
      // check whether there is a ".javaopts" file
      val argsFile  = new File(logFile.getParentFile, fileBase + ".javaopts")
      val argString = file2String(argsFile)
      if (argString != "")
        NestUI.verbose("Found javaopts file '%s', using options: '%s'".format(argsFile, argString))

      val testFullPath = {
        val d = new File(logFile.getParentFile, fileBase)
        if (d.isDirectory) d.getAbsolutePath
        else {
          val f = new File(logFile.getParentFile, fileBase + ".scala")
          if (f.isFile) f.getAbsolutePath
          else ""
        }
      }

      // Note! As this currently functions, JAVA_OPTS must precede argString
      // because when an option is repeated to java only the last one wins.
      // That means until now all the .javaopts files were being ignored because
      // they all attempt to change options which are also defined in
      // partest.java_opts, leading to debug output like:
      //
      // debug: Found javaopts file 'files/shootout/message.scala-2.javaopts', using options: '-Xss32k'
      // debug: java -Xss32k -Xss2m -Xms256M -Xmx1024M -classpath [...]
      val extras = if (isPartestDebug) List("-Dpartest.debug=true") else Nil
      val propertyOptions = List(
        "-Dfile.encoding=UTF-8",
        "-Djava.library.path="+logFile.getParentFile.getAbsolutePath,
        "-Dpartest.output="+outDir.getAbsolutePath,
        "-Dpartest.lib="+LATEST_LIB,
        "-Dpartest.reflect="+LATEST_REFLECT,
        "-Dpartest.comp="+LATEST_COMP,
        "-Dpartest.cwd="+outDir.getParent,
        "-Dpartest.test-path="+testFullPath,
        "-Dpartest.testname="+fileBase,
        "-Djavacmd="+javaCmd,
        "-Djavaccmd="+javacCmd,
        "-Duser.language=en",
        "-Duser.country=US"
      ) ++ extras

      val classpath = if (classpathPrefix != "") join(classpathPrefix, CLASSPATH) else CLASSPATH
      val cmd = javaCmd +: (
        (JAVA_OPTS.split(' ') ++ javaOpts.split(' ') ++ argString.split(' ')).map(_.trim).filter(_ != "") ++ Seq(
          "-classpath",
          join(outDir.toString, classpath)
        ) ++ propertyOptions ++ Seq(
          "scala.tools.nsc.MainGenericRunner",
          "-usejavacp",
          "Test",
          "jvm"
        )
      )

      runCommand(cmd, logFile)
    }

    private def getCheckFilePath(dir: File, suffix: String = "") = {
      def chkFile(s: String) = (Directory(dir) / "%s%s.check".format(fileBase, s)).toFile

      if (chkFile("").isFile || suffix == "") chkFile("")
      else chkFile("-" + suffix)
    }
    private def getCheckFile(dir: File) = Some(getCheckFilePath(dir, kind)) filter (_.canRead)

    private def compareOutput(dir: File, logFile: File): String = {
      val checkFile = getCheckFilePath(dir, kind)
      val diff =
        if (checkFile.canRead) compareFiles(logFile, checkFile.jfile)
        else file2String(logFile)

      // if check file exists, compare with log file
      if (diff != "" && fileManager.updateCheck) {
        NestUI.verbose("Updating checkfile " + checkFile.jfile)
        val toWrite = if (checkFile.exists) checkFile else getCheckFilePath(dir, "")
        toWrite writeAll file2String(logFile)
        ""
      }
      else diff
    }

    def newTestWriters() = {
      val swr = new StringWriter
      val wr  = new PrintWriter(swr, true)
      // diff    = ""

      ((swr, wr))
    }

    def fail(what: Any) = {
      NestUI.verbose("scalac: compilation of "+what+" failed\n")
      false
    }
    def diffCheck(testFile: File, diff: String) = {
      testDiff = diff
      testDiff == ""
    }

    /** 1. Creates log file and output directory.
     *  2. Runs script function, providing log file and output directory as arguments.
     */
    def runInContext(file: File, script: (File, File) => Boolean): (Boolean, LogContext) = {
      val (swr, wr) = newTestWriters()
      printInfoStart(file, wr)

      NestUI.verbose(this+" running test "+fileBase)
      val outDir = createOutputDir()
      NestUI.verbose("output directory: "+outDir)

      // run test-specific code
      val succeeded = try {
        if (isPartestDebug) {
          val (result, millis) = timed(script(logFile, outDir))
          fileManager.recordTestTiming(file.getPath, millis)
          result
        }
        else script(logFile, outDir)
      }
      catch exHandler(logFile, "", false)

      (succeeded, LogContext(logFile, swr, wr))
    }

    def groupedFiles(dir: File): List[List[File]] = {
      val testFiles = dir.listFiles.toList filter isJavaOrScala

      def isInGroup(f: File, num: Int) = SFile(f).stripExtension endsWith ("_" + num)
      val groups = (0 to 9).toList map (num => (testFiles filter (f => isInGroup(f, num))).sorted)
      val noGroupSuffix = (testFiles filterNot (groups.flatten contains)).sorted

      noGroupSuffix :: groups filterNot (_.isEmpty)
    }

    def compileFilesIn(dir: File, logFile: File, outDir: File): CompilationOutcome = {
      def compileGroup(g: List[File]): CompilationOutcome = {
        val (scalaFiles, javaFiles) = g partition isScala
        val allFiles = javaFiles ++ scalaFiles

        List(1, 2, 3).foldLeft(CompileSuccess: CompilationOutcome) {
          case (CompileSuccess, 1) if scalaFiles.nonEmpty => compileMgr.attemptCompile(Some(outDir), allFiles, kind, logFile)     // java + scala
          case (CompileSuccess, 2) if javaFiles.nonEmpty  => javac(outDir, javaFiles, logFile)                                    // java
          case (CompileSuccess, 3) if scalaFiles.nonEmpty => compileMgr.attemptCompile(Some(outDir), scalaFiles, kind, logFile)   // scala
          case (outcome, _)                               => outcome
        }
      }
      groupedFiles(dir).foldLeft(CompileSuccess: CompilationOutcome) {
        case (CompileSuccess, files) => compileGroup(files)
        case (outcome, _)            => outcome
      }
    }

    def runTestCommon(file: File, expectFailure: Boolean)(
      onSuccess: (File, File) => Boolean,
      onFail: (File, File) => Unit = (_, _) => ()): (Boolean, LogContext) =
    {
      runInContext(file, (logFile: File, outDir: File) => {
        val outcome = (
          if (file.isDirectory) compileFilesIn(file, logFile, outDir)
          else compileMgr.attemptCompile(None, List(file), kind, logFile)
        )
        val result = (
          if (expectFailure) outcome.isNegative
          else outcome.isPositive
        )

        if (result) onSuccess(logFile, outDir)
        else { onFail(logFile, outDir) ; false }
      })
    }

    def runJvmTest(file: File): (Boolean, LogContext) =
      runTestCommon(file, expectFailure = false)((logFile, outDir) => {
        val dir      = file.getParentFile

        // adding codelib.jar to the classpath
        // codelib provides the possibility to override standard reify
        // this shields the massive amount of reification tests from changes in the API
        execTest(outDir, logFile, PathSettings.srcCodeLib.toString) && {
          // cannot replace paths here since this also inverts slashes
          // which affects a bunch of tests
          //fileManager.mapFile(logFile, replaceSlashes(dir, _))
          diffCheck(file, compareOutput(dir, logFile))
        }
      })

    // Apache Ant 1.6 or newer
    def ant(args: Seq[String], output: File): Boolean = {
      val antDir = Directory(envOrElse("ANT_HOME", "/opt/ant/"))
      val antLibDir = Directory(antDir / "lib")
      val antLauncherPath = SFile(antLibDir / "ant-launcher.jar").path
      val antOptions =
        if (NestUI._verbose) List("-verbose", "-noinput")
        else List("-noinput")
      val cmd = javaCmd +: (
        JAVA_OPTS.split(' ').map(_.trim).filter(_ != "") ++ Seq(
          "-classpath",
          antLauncherPath,
          "org.apache.tools.ant.launch.Launcher"
        ) ++ antOptions ++ args
      )

      try runCommand(cmd, output)
      catch exHandler(output, "ant command '" + cmd + "' failed:\n", false)
    }

    def runAntTest(file: File): (Boolean, LogContext) = {
      val (swr, wr) = newTestWriters()
      printInfoStart(file, wr)

      NestUI.verbose(this+" running test "+fileBase)

      val succeeded = try {
        val binary = "-Dbinary="+(
          if      (fileManager.LATEST_LIB endsWith "build/quick/classes/library") "quick"
          else if (fileManager.LATEST_LIB endsWith "build/pack/lib/scala-library.jar") "pack"
          else if (fileManager.LATEST_LIB endsWith "dists/latest/lib/scala-library.jar/") "latest"
          else "installed"
        )
        val args = Array(binary, "-logfile", logFile.path, "-file", file.path)
        NestUI.verbose("ant "+args.mkString(" "))
        ant(args, logFile) && diffCheck(file, compareOutput(file.getParentFile, logFile))
      }
      catch { // *catch-all*
        case e: Exception =>
          NestUI.verbose("caught "+e)
          false
      }

      (succeeded, LogContext(logFile, swr, wr))
    }

    def runSpecializedTest(file: File): (Boolean, LogContext) =
      runTestCommon(file, expectFailure = false)((logFile, outDir) => {
        val dir       = file.getParentFile

        // adding the instrumented library to the classpath
        ( execTest(outDir, logFile, PathSettings.srcSpecLib.toString) &&
          diffCheck(file, compareOutput(dir, logFile))
        )
      })

    def runInstrumentedTest(file: File): (Boolean, LogContext) =
      runTestCommon(file, expectFailure = false)((logFile, outDir) => {
        val dir       = file.getParentFile

        // adding the javagent option with path to instrumentation agent
        execTest(outDir, logFile, javaOpts = "-javaagent:"+PathSettings.instrumentationAgentLib) &&
        diffCheck(file, compareOutput(dir, logFile))
      })

    def processSingleFile(file: File): (Boolean, LogContext) = kind match {
      case "scalacheck" =>
        val succFn: (File, File) => Boolean = { (logFile, outDir) =>
          NestUI.verbose("compilation of "+file+" succeeded\n")

          val outURL    = outDir.getAbsoluteFile.toURI.toURL
          val logWriter = new PrintStream(new FileOutputStream(logFile), true)

          Output.withRedirected(logWriter) {
            // this classloader is test specific: its parent contains library classes and others
            ScalaClassLoader.fromURLs(List(outURL), params.scalaCheckParentClassLoader).run("Test", Nil)
          }

          NestUI.verbose(file2String(logFile))
          // obviously this must be improved upon
          val lines = SFile(logFile).lines map (_.trim) filterNot (_ == "") toBuffer;
          lines.forall(x => !x.startsWith("!")) || {
            NestUI.normal("ScalaCheck test failed. Output:\n")
            lines foreach (x => NestUI.normal(x + "\n"))
            false
          }
        }
        runTestCommon(file, expectFailure = false)(
          succFn,
          (logFile, outDir) => outputLogFile(logFile)
        )

      case "pos" =>
        runTestCommon(file, expectFailure = false)(
          (logFile, outDir) => true,
          (_, _) => ()
        )

      case "neg" =>
        runTestCommon(file, expectFailure = true)((logFile, outDir) => {
          // compare log file to check file
          val dir      = file.getParentFile

          // diff is contents of logFile
          fileManager.mapFile(logFile, replaceSlashes(dir, _))
          diffCheck(file, compareOutput(dir, logFile))
        })

      case "run" | "jvm" =>
        runJvmTest(file)

      case "specialized" =>
        runSpecializedTest(file)

      case "instrumented" =>
        runInstrumentedTest(file)

      case "presentation" =>
        runJvmTest(file) // for the moment, it's exactly the same as for a run test

      case "ant" =>
        runAntTest(file)

      case "buildmanager" =>
        val (swr, wr) = newTestWriters()
        printInfoStart(file, wr)
        val (outDir, testFile, changesDir) = {
          if (!file.isDirectory)
            (null, null, null)
          else {
            NestUI.verbose(this+" running test "+fileBase)
            val outDir = createOutputDir()
            val testFile = new File(file, fileBase + ".test")
            val changesDir = new File(file, fileBase + ".changes")

            if (changesDir.isFile || !testFile.isFile) {
              // if changes exists then it has to be a dir
              if (!testFile.isFile) NestUI.verbose("invalid build manager test file")
              if (changesDir.isFile) NestUI.verbose("invalid build manager changes directory")
              (null, null, null)
            }
            else {
              copyTestFiles(file, outDir)
              NestUI.verbose("outDir:  "+outDir)
              NestUI.verbose("logFile: "+logFile)
              (outDir, testFile, changesDir)
            }
          }
        }
        if (outDir == null)
          return (false, LogContext(logFile))

        // Pre-conditions satisfied
        val sourcepath = outDir.getAbsolutePath+File.separator

        // configure input/output files
        val logWriter = new PrintStream(new FileOutputStream(logFile), true)
        val testReader = new BufferedReader(new FileReader(testFile))
        val logConsoleWriter = new PrintWriter(logWriter, true)

        // create proper settings for the compiler
        val settings = new Settings(workerError)
        settings.outdir.value = outDir.getAbsoluteFile.getAbsolutePath
        settings.sourcepath.value = sourcepath
        settings.classpath.value = fileManager.CLASSPATH
        settings.Ybuildmanagerdebug.value = true

        // simulate Build Manager loop
        val prompt = "builder > "
        val reporter = new ConsoleReporter(settings, scala.Console.in, logConsoleWriter)
        val bM: BuildManager =
            new RefinedBuildManager(settings) {
              override protected def newCompiler(settings: Settings) =
                  new BuilderGlobal(settings, reporter)
            }

        def testCompile(line: String): Boolean = {
          NestUI.verbose("compiling " + line)
          val args = (line split ' ').toList
          val command = new CompilerCommand(args, settings)
          command.ok && {
            bM.update(filesToSet(settings.sourcepath.value, command.files), Set.empty)
            !reporter.hasErrors
          }
        }

        val updateFiles = (line: String) => {
          NestUI.verbose("updating " + line)
          (line split ' ').toList forall (u =>
            (u split "=>").toList match {
                case origFileName::(newFileName::Nil) =>
                  val newFile = new File(changesDir, newFileName)
                  if (newFile.isFile) {
                    val v = overwriteFileWith(new File(outDir, origFileName), newFile)
                    if (!v)
                      NestUI.verbose("'update' operation on " + u + " failed")
                    v
                  } else {
                    NestUI.verbose("File " + newFile + " is invalid")
                    false
                  }
                case a =>
                  NestUI.verbose("Other =: " + a)
                  false
            }
          )
        }

        def loop(): Boolean = {
          testReader.readLine() match {
            case null | ""    =>
              NestUI.verbose("finished")
              true
            case s if s startsWith ">>update "  =>
              updateFiles(s stripPrefix ">>update ") && loop()
            case s if s startsWith ">>compile " =>
              val files = s stripPrefix ">>compile "
              logWriter.println(prompt + files)
              // In the end, it can finish with an error
              if (testCompile(files)) loop()
              else {
                val t = testReader.readLine()
                (t == null) || (t == "")
              }
            case s =>
              NestUI.verbose("wrong command in test file: " + s)
              false
          }
        }

        Output.withRedirected(logWriter) {
          try loop()
          finally testReader.close()
        }
        fileManager.mapFile(logFile, replaceSlashes(new File(sourcepath), _))

        (diffCheck(file, compareOutput(file, logFile)), LogContext(logFile, swr, wr))

      case "res" => {
          // simulate resident compiler loop
          val prompt = "\nnsc> "

          val (swr, wr) = newTestWriters()
          printInfoStart(file, wr)

          NestUI.verbose(this+" running test "+fileBase)
          val dir = file.getParentFile
          val outDir = createOutputDir()
          val resFile = new File(dir, fileBase + ".res")
          NestUI.verbose("outDir:  "+outDir)
          NestUI.verbose("logFile: "+logFile)
          //NestUI.verbose("logFileErr: "+logFileErr)
          NestUI.verbose("resFile: "+resFile)

          // run compiler in resident mode
          // $SCALAC -d "$os_dstbase".obj -Xresident -sourcepath . "$@"
          val sourcedir  = logFile.getParentFile.getAbsoluteFile
          val sourcepath = sourcedir.getAbsolutePath+File.separator
          NestUI.verbose("sourcepath: "+sourcepath)

          val argList = List(
            "-d", outDir.getAbsoluteFile.getPath,
            "-Xresident",
            "-sourcepath", sourcepath)

          // configure input/output files
          val logOut    = new FileOutputStream(logFile)
          val logWriter = new PrintStream(logOut, true)
          val resReader = new BufferedReader(new FileReader(resFile))
          val logConsoleWriter = new PrintWriter(new OutputStreamWriter(logOut), true)

          // create compiler
          val settings = new Settings(workerError)
          settings.sourcepath.value = sourcepath
          settings.classpath.value = fileManager.CLASSPATH
          val reporter = new ConsoleReporter(settings, scala.Console.in, logConsoleWriter)
          val command = new CompilerCommand(argList, settings)
          object compiler extends Global(command.settings, reporter)

          val resCompile = (line: String) => {
            NestUI.verbose("compiling "+line)
            val cmdArgs = (line split ' ').toList map (fs => new File(dir, fs).getAbsolutePath)
            NestUI.verbose("cmdArgs: "+cmdArgs)
            val sett = new Settings(workerError)
            sett.sourcepath.value = sourcepath
            val command = new CompilerCommand(cmdArgs, sett)
            command.ok && {
              (new compiler.Run) compile command.files
              !reporter.hasErrors
            }
          }

          def loop(action: String => Boolean): Boolean = {
            logWriter.print(prompt)
            resReader.readLine() match {
              case null | ""  => logWriter.flush() ; true
              case line       => action(line) && loop(action)
            }
          }

          Output.withRedirected(logWriter) {
            try loop(resCompile)
            finally resReader.close()
          }
          fileManager.mapFile(logFile, replaceSlashes(dir, _))

          (diffCheck(file, compareOutput(dir, logFile)), LogContext(logFile, swr, wr))
        }

      case "shootout" =>
        val (swr, wr) = newTestWriters()
        printInfoStart(file, wr)

        NestUI.verbose(this+" running test "+fileBase)
        val outDir = createOutputDir()

        // 2. define file {outDir}/test.scala that contains code to compile/run
        val testFile = new File(outDir, "test.scala")
        NestUI.verbose("outDir:   "+outDir)
        NestUI.verbose("logFile:  "+logFile)
        NestUI.verbose("testFile: "+testFile)

        // 3. cat {test}.scala.runner {test}.scala > testFile
        val runnerFile = new File(parent, fileBase+".scala.runner")
        val bodyFile   = new File(parent, fileBase+".scala")
        SFile(testFile).writeAll(
          file2String(runnerFile),
          file2String(bodyFile)
        )

        // 4. compile testFile
        val ok = compileMgr.attemptCompile(None, List(testFile), kind, logFile) eq CompileSuccess
        NestUI.verbose("compilation of " + testFile + (if (ok) "succeeded" else "failed"))
        val result = ok && {
          execTest(outDir, logFile) && {
            NestUI.verbose(this+" finished running "+fileBase)
            diffCheck(file, compareOutput(parent, logFile))
          }
        }

        (result, LogContext(logFile, swr, wr))

      case "scalap" =>
        runInContext(file, (logFile: File, outDir: File) => {
          val sourceDir = Directory(if (file.isFile) file.getParent else file)
          val sources   = sourceDir.files filter (_ hasExtension "scala") map (_.jfile) toList
          val results   = sourceDir.files filter (_.name == "result.test") map (_.jfile) toList

          if (sources.length != 1 || results.length != 1) {
            NestUI.warning("Misconfigured scalap test directory: " + sourceDir + " \n")
            false
          }
          else {
            val resFile = results.head
            // 2. Compile source file

            if (!compileMgr.attemptCompile(Some(outDir), sources, kind, logFile).isPositive) {
              NestUI.normal("compilerMgr failed to compile %s to %s".format(sources mkString ", ", outDir))
              false
            }
            else {
              // 3. Decompile file and compare results
              val isPackageObject = sourceDir.name startsWith "package"
              val className       = sourceDir.name.capitalize + (if (!isPackageObject) "" else ".package")
              val url             = outDir.toURI.toURL
              val loader          = ScalaClassLoader.fromURLs(List(url), this.getClass.getClassLoader)
              val clazz           = loader.loadClass(className)

              val byteCode = ByteCode.forClass(clazz)
              val result   = scala.tools.scalap.Main.decompileScala(byteCode.bytes, isPackageObject)

              SFile(logFile) writeAll result
              diffCheck(file, compareFiles(logFile, resFile))
            }
          }
        })

      case "script" =>
        val (swr, wr) = newTestWriters()
        printInfoStart(file, wr)

        NestUI.verbose(this+" running test "+fileBase)

        // check whether there is an args file
        val argsFile = new File(file.getParentFile, fileBase+".args")
        NestUI.verbose("argsFile: "+argsFile)
        val argString = file2String(argsFile)
        val succeeded = try {
          val cmdString =
            if (isWin) {
              val batchFile = new File(file.getParentFile, fileBase+".bat")
              NestUI.verbose("batchFile: "+batchFile)
              batchFile.getAbsolutePath
            }
            else file.getAbsolutePath

          val ok = runCommand(cmdString+argString, logFile)
          ( ok && diffCheck(file, compareOutput(file.getParentFile, logFile)) )
        }
        catch { case e: Exception => NestUI.verbose("caught "+e) ; false }

        (succeeded, LogContext(logFile, swr, wr))
    }

    private def crashContext(t: Throwable): LogContext = {
      try {
        logStackTrace(logFile, t, "Possible compiler crash during test of: " + testFile + "\n")
        LogContext(logFile)
      }
      catch { case t: Throwable => LogContext(null) }
    }

    def run(): (Boolean, LogContext) = {
      val result = try processSingleFile(testFile) catch { case t: Throwable => (false, crashContext(t)) }
      passed = Some(result._1)
      result
    }

    def reportResult(writers: Option[(StringWriter, PrintWriter)]) {
      writers foreach { case (swr, wr) =>
        if (passed.isEmpty) printInfoTimeout(wr)
        else printInfoEnd(passed.get, wr)
        wr.flush()
        swr.flush()
        NestUI.normal(swr.toString)

        if (passed exists (x => !x)) {
          if (fileManager.showDiff || isPartestDebug)
            NestUI.normal(testDiff)
          if (fileManager.showLog)
            showLog(logFile)
        }
      }
      toDelete foreach (_.deleteRecursively())
    }
  }

  def runTest(testFile: File): TestState = {
    val runner = new Runner(testFile)
    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    if (fileManager.failed && !runner.logFile.canRead)
      return TestState.Ok

    // sys addShutdownHook cleanup()
    val ((success, ctx), elapsed) = timed(runner.run())
    val state                     = if (success) TestState.Ok else TestState.Fail

    runner.reportResult(ctx.writers)
    state
  }

  private def filesToSet(pre: String, fs: List[String]): Set[AbstractFile] =
    fs flatMap (s => Option(AbstractFile getFile (pre + s))) toSet

  private def copyTestFiles(testDir: File, destDir: File) {
    val invalidExts = List("changes", "svn", "obj")
    testDir.listFiles.toList filter (
            f => (isJavaOrScala(f) && f.isFile) ||
                 (f.isDirectory && !(invalidExts.contains(SFile(f).extension)))) foreach
      { f => fileManager.copyFile(f, destDir) }
  }

  private def showLog(logFile: File) {
    file2String(logFile) match {
      case "" if logFile.canRead  => ()
      case ""                     => NestUI.failure("Couldn't open log file: " + logFile + "\n")
      case s                      => NestUI.normal(s)
    }
  }
}
