/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io._
import java.net.{ URLClassLoader, URL }
import java.util.{ Timer, TimerTask }

import scala.util.Properties.{ isWin }
import scala.tools.nsc.{ ObjectRunner, Settings, CompilerCommand, Global }
import scala.tools.nsc.io.{ AbstractFile, PlainFile, Path, Directory, File => SFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ ClassPath, FakePos, ScalaClassLoader }
import ClassPath.{ join, split }

import scala.actors.{ Actor, Exit, TIMEOUT }
import scala.actors.Actor._
import scala.tools.scalap.scalax.rules.scalasig.{ByteCode, ClassFileParser, ScalaSigAttributeParsers}

import scala.collection.immutable.{ HashMap, Map => ImmMap }
import scala.collection.Map

import scala.tools.nsc.interactive.{BuildManager, RefinedBuildManager}

case class RunTests(kind: String, files: List[File])
case class Results(results: ImmMap[String, Int], logs: List[LogFile], outdirs: List[File])

case class LogContext(file: LogFile, writers: Option[(StringWriter, PrintWriter)])

abstract class TestResult {
  def file: File
}
case class Result(override val file: File, context: LogContext) extends TestResult
case class Timeout(override val file: File) extends TestResult

class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}

class ScalaCheckFileManager(val origmanager: FileManager) extends FileManager {
  def testRootDir: Directory = origmanager.testRootDir
  def testRootPath: String = origmanager.testRootPath

  var JAVACMD: String = origmanager.JAVACMD
  var JAVAC_CMD: String = origmanager.JAVAC_CMD

  var CLASSPATH: String = join(origmanager.CLASSPATH, PathSettings.scalaCheck.path)
  var LATEST_LIB: String = origmanager.LATEST_LIB
}

class Worker(val fileManager: FileManager, scalaCheckParentClassLoader: ScalaClassLoader) extends Actor {
  import fileManager._

  val scalaCheckFileManager = new ScalaCheckFileManager(fileManager)
  var reporter: ConsoleReporter = _
  val timer = new Timer

  def error(msg: String): Unit = reporter.error(
    FakePos("scalac"),
    msg + "\n  scalac -help  gives more information"
  )

  def act() {
    react {
      case RunTests(kind, files) =>
        //NestUI.normal("received "+files.length+" to test")
        val master = sender
        runTests(kind, files) { results =>
          master ! Results(results, createdLogFiles, createdOutputDirs)
        }
    }
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
    NestUI.normal("[...]%s%s".format(name, " " * (totalWidth - name.length)), printer)
  }

  def printInfoEnd(success: Boolean, printer: PrintWriter) {
    NestUI.normal("[", printer)
    if (success) NestUI.success("  OK  ", printer)
    else NestUI.failure("FAILED", printer)
    NestUI.normal("]\n", printer)
  }

  def printInfoTimeout(printer: PrintWriter) {
    NestUI.normal("[", printer)
    NestUI.failure("TIMOUT", printer)
    NestUI.normal("]\n", printer)
  }

  var log = ""
  var createdLogFiles: List[LogFile] = Nil
  var createdOutputDirs: List[File] = Nil

  def createLogFile(file: File, kind: String): LogFile = {
    val logFile = fileManager.getLogFile(file, kind)
    createdLogFiles ::= logFile
    logFile
  }

  def createOutputDir(dir: File, fileBase: String, kind: String): File = {
    val outDir = Path(dir) / Directory("%s-%s.obj".format(fileBase, kind))
    outDir.createDirectory()
    createdOutputDirs ::= outDir.jfile
    outDir.jfile
  }

  /* Note: not yet used/tested. */
  // def execTestObjectRunner(file: File, outDir: File, logFile: File) {
  //   val consFM = new ConsoleFileManager
  //
  //   val classpath: List[URL] = {
  //     import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
  //     val units = (
  //       List(outDir, latestCompFile, latestLibFile, latestPartestFile) :::
  //       ((CLASSPATH split File.pathSeparatorChar).toList map (x => new File(x)))
  //     )
  //     units map (_.toURI.toURL)
  //   }
  //
  //   NestUI.verbose("ObjectRunner classpath: "+classpath)
  //
  //   try {
  //     // configure input/output files
  //     val logOut    = new FileOutputStream(logFile)
  //     val logWriter = new PrintStream(logOut)
  //
  //     // grab global lock
  //     fileManager.synchronized {
  //       withOutputRedirected(logWriter) {
  //         System.setProperty("java.library.path", logFile.getParentFile.getCanonicalFile.getAbsolutePath)
  //         System.setProperty("partest.output", outDir.getCanonicalFile.getAbsolutePath)
  //         System.setProperty("partest.lib", LATEST_LIB)
  //         System.setProperty("partest.cwd", outDir.getParent)
  //         ObjectRunner.run(classpath, "Test", List("jvm"))
  //       }
  //     }
  //
  //     /*val out = new FileOutputStream(logFile, true)
  //     Console.withOut(new PrintStream(out)) {
  //       ObjectRunner.run(classpath, "Test", List("jvm"))
  //     }
  //     out.flush
  //     out.close*/
  //   } catch {
  //     case e: Exception =>
  //       NestUI.verbose(e+" ("+file.getPath+")")
  //       e.printStackTrace()
  //   }
  // }

  def javac(outDir: File, files: List[File], output: File): Boolean = {
    // compile using command-line javac compiler
    val javacCmd = if ((fileManager.JAVAC_CMD.indexOf("${env.JAVA_HOME}") != -1) ||
                       fileManager.JAVAC_CMD.equals("/bin/javac") ||
                       fileManager.JAVAC_CMD.equals("\\bin\\javac"))
      "javac"
    else
      fileManager.JAVAC_CMD

    val cmd = javacCmd+
      " -d "+outDir.getAbsolutePath+
      " -classpath "+ join(outDir.toString, CLASSPATH) +
      " "+files.mkString(" ")

    val (success, msg) = try {
      val exitCode = runCommand(cmd, output)
      NestUI.verbose("javac returned exit code: "+exitCode)
      if (exitCode != 0)
        (false, "Running \"javac\" failed with exit code: "+exitCode+"\n"+cmd+"\n")
      else
        (true, "")
    } catch {
      case e: Exception =>
        val swriter = new StringWriter
        e.printStackTrace(new PrintWriter(swriter))
        (false, "Running \"javac\" failed:\n"+cmd+"\n"+swriter.toString+"\n")
    }
    if (!success) {
      val writer = new PrintWriter(new FileWriter(output, true), true)
      writer.print(msg)
      writer.close()
    }
    success
  }

  /** Runs <code>command</code> redirecting standard out and
   *  error out to <code>output</code> file.
   */
  def runCommand(command: String, output: File): Int = {
    NestUI.verbose("running command:\n"+command)
    val proc = Runtime.getRuntime.exec(command)
    val in = proc.getInputStream
    val err = proc.getErrorStream
    val writer = new PrintWriter(new FileWriter(output), true)
    val inApp = StreamAppender(in, writer)
    val errApp = StreamAppender(err, writer)
    val async = new Thread(errApp)
    async.start()
    inApp.run()
    async.join()
    writer.close()

    try proc.exitValue()
    catch { case _: IllegalThreadStateException => 0 }
  }

  def execTest(outDir: File, logFile: File, fileBase: String) {
    // check whether there is a ".javaopts" file
    val argsFile = new File(logFile.getParentFile, fileBase+".javaopts")
    val argString = if (argsFile.exists) {
      NestUI.verbose("Found javaopts file: "+argsFile)
      val fileReader = new FileReader(argsFile)
      val reader = new BufferedReader(fileReader)
      val options = reader.readLine()
      reader.close()
      NestUI.verbose("Found javaopts file '%s', using options: '%s'".format(argsFile, options))
      options
    } else ""

    def quote(path: String) = "\""+path+"\""

    // Note! As this currently functions, JAVA_OPTS must precede argString
    // because when an option is repeated to java only the last one wins.
    // That means until now all the .javaopts files were being ignored because
    // they all attempt to change options which are also defined in
    // partest.java_opts, leading to debug output like:
    //
    // debug: Found javaopts file 'files/shootout/message.scala-2.javaopts', using options: '-Xss32k'
    // debug: java -Xss32k -Xss2m -Xms256M -Xmx1024M -classpath [...]
    val propertyOptions = List(
      "-Djava.library.path="+logFile.getParentFile.getAbsolutePath,
      "-Dpartest.output="+outDir.getAbsolutePath,
      "-Dpartest.lib="+LATEST_LIB,
      "-Dpartest.cwd="+outDir.getParent,
      "-Djavacmd="+JAVACMD,
      "-Duser.language=en -Duser.country=US"
    ) ::: (
      if (isPartestDebug) List("-Dpartest.debug=true") else Nil
    )

    val cmd = (
      List(
        JAVACMD,
        JAVA_OPTS,
        argString,
        "-classpath " + join(outDir.toString, CLASSPATH)
      ) ::: propertyOptions ::: List(
        "scala.tools.nsc.MainGenericRunner",
        "-usejavacp",
        "Test",
        "jvm"
      )
    ) mkString " "

    runCommand(cmd, logFile)

    if (fileManager.showLog) {
      // produce log as string in `log`
      val reader = new BufferedReader(new FileReader(logFile))
      val swriter = new StringWriter
      val pwriter = new PrintWriter(swriter, true)
      val appender = new StreamAppender(reader, pwriter)
      appender.run()
      log = swriter.toString
    }
  }

  def getCheckFile(dir: File, fileBase: String, kind: String) = {
    def chkFile(s: String) = Directory(dir) / "%s%s.check".format(fileBase, s)
    val checkFile = if (chkFile("").isFile) chkFile("") else chkFile("-" + kind)

    if (checkFile.canRead) Some(checkFile) else None
  }

  def existsCheckFile(dir: File, fileBase: String, kind: String) =
    getCheckFile(dir, fileBase, kind).isDefined

  def compareOutput(dir: File, fileBase: String, kind: String, logFile: File): String =
    // if check file exists, compare with log file
    getCheckFile(dir, fileBase, kind) match {
      case Some(f)  => fileManager.compareFiles(logFile, f.jfile)
      case _        => file2String(logFile)
    }

  def file2String(logFile: File) = SFile(logFile).slurp()
  def isJava(f: File) = SFile(f) hasExtension "java"
  def isScala(f: File) = SFile(f) hasExtension "scala"
  def isJavaOrScala(f: File) = isJava(f) || isScala(f)

  /** Runs a list of tests.
   *
   * @param kind  The test kind (pos, neg, run, etc.)
   * @param files The list of test files
   */
  def runTests(kind: String, files: List[File])(topcont: ImmMap[String, Int] => Unit) {
    val compileMgr = new CompileManager(fileManager)
    if (kind == "scalacheck") fileManager.CLASSPATH += File.pathSeparator + PathSettings.scalaCheck

    var errors = 0
    var succeeded = true
    var diff = ""
    var log = ""

    def fail(what: Any) {
      NestUI.verbose("scalac: compilation of "+what+" failed\n")
      succeeded = false
    }
    def diffCheck(latestDiff: String) = {
      diff = latestDiff
      if (latestDiff != "") {
        NestUI.verbose("output differs from log file\n")
        succeeded = false
      }
    }

    /** 1. Creates log file and output directory.
     *  2. Runs <code>script</code> function, providing log file and
     *     output directory as arguments.
     */
    def runInContext(file: File, kind: String, script: (File, File) => Unit): LogContext = {
      // when option "--failed" is provided
      // execute test only if log file is present
      // (which means it failed before)
      val logFile = createLogFile(file, kind)
      if (!fileManager.failed || logFile.canRead) {
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
        NestUI.verbose("output directory: "+outDir)

        // run test-specific code
        try {
          if (isPartestDebug) {
            val t1 = System.currentTimeMillis
            script(logFile, outDir)
            val t2 = System.currentTimeMillis
            fileManager.recordTestTiming(file.getPath, t2 - t1)
          }
          else {
            script(logFile, outDir)
          }
        } catch {
          case e: Exception =>
            val writer = new PrintWriter(new FileWriter(logFile), true)
            e.printStackTrace(writer)
            writer.close()
            succeeded = false
        }

        LogContext(logFile, Some((swr, wr)))
      } else
        LogContext(logFile, None)
    }

    def compileFilesIn(dir: File, kind: String, logFile: File, outDir: File) {
      val testFiles = dir.listFiles.toList filter isJavaOrScala

      def isInGroup(f: File, num: Int) = SFile(f).stripExtension endsWith ("_" + num)
      val groups = (0 to 9).toList map (num => testFiles filter (f => isInGroup(f, num)))
      val noGroupSuffix = testFiles filterNot (groups.flatten contains)

      def compileGroup(g: List[File]) {
        val (scalaFiles, javaFiles) = g partition isScala

        if (scalaFiles.nonEmpty) {
          if (!compileMgr.shouldCompile(outDir, javaFiles ::: scalaFiles, kind, logFile))
            fail(g)
        }

        if (succeeded && javaFiles.nonEmpty) {
          succeeded = javac(outDir, javaFiles, logFile)
          if (succeeded && scalaFiles.nonEmpty && !compileMgr.shouldCompile(outDir, scalaFiles, kind, logFile))
            fail(scalaFiles)
        }
      }

      if (noGroupSuffix.nonEmpty)
        compileGroup(noGroupSuffix)

      groups foreach (grp => if (succeeded) compileGroup(grp))
    }

    def failCompileFilesIn(dir: File, kind: String, logFile: File, outDir: File) {
      val testFiles   = dir.listFiles.toList
      val sourceFiles = testFiles filter isJavaOrScala

      if (sourceFiles.nonEmpty) {
        if (!compileMgr.shouldFailCompile(outDir, sourceFiles, kind, logFile))
          fail(testFiles filter isScala)
      }
    }

    def runTestCommon(file: File, kind: String, expectFailure: Boolean)(onSuccess: (File, File) => Unit): LogContext =
      runInContext(file, kind, (logFile: File, outDir: File) => {

        if (file.isDirectory) {
          val f = if (expectFailure) failCompileFilesIn _ else compileFilesIn _
          f(file, kind, logFile, outDir)
        }
        else {
          val f: (List[File], String, File) => Boolean =
            if (expectFailure) compileMgr.shouldFailCompile _
            else compileMgr.shouldCompile _

          if (!f(List(file), kind, logFile))
            fail(file)
        }

        if (succeeded)  // run test
          onSuccess(logFile, outDir)
      })

    def runJvmTest(file: File, kind: String): LogContext =
      runTestCommon(file, kind, expectFailure = false)((logFile, outDir) => {
        val fileBase = basename(file.getName)
        val dir      = file.getParentFile

        //TODO: detect whether we have to use Runtime.exec
        // val useRuntime = true
        //
        // if (useRuntime)
        //   execTest(outDir, logFile, fileBase)
        // else
        //   execTestObjectRunner(file, outDir, logFile)
        // // NestUI.verbose(this+" finished running "+fileBase)
        execTest(outDir, logFile, fileBase)

        diffCheck(compareOutput(dir, fileBase, kind, logFile))
      })

    def processSingleFile(file: File): LogContext = kind match {
      case "scalacheck" =>
        runTestCommon(file, kind, expectFailure = false)((logFile, outDir) => {
          NestUI.verbose("compilation of "+file+" succeeded\n")

          val outURL = outDir.getCanonicalFile.toURI.toURL

          val logWriter = new PrintStream(new FileOutputStream(logFile))

          withOutputRedirected(logWriter) {
            // this classloader is test specific
            // its parent contains library classes and others
            val classloader = ScalaClassLoader.fromURLs(List(outURL), scalaCheckParentClassLoader)
            classloader.run("Test", Nil)
          }

          NestUI.verbose(SFile(logFile).slurp())
          // obviously this must be improved upon
          succeeded = {
            val lines = SFile(logFile).lines.filter(_.trim != "").toBuffer
            val failures = lines filter (_ startsWith "!")
            val passedok = lines filter (_ startsWith "+") forall (_ contains "OK")
            failures.isEmpty && passedok
          }
          NestUI.verbose("test for '" + file + "' success: " + succeeded)
        })

      case "pos" =>
        runTestCommon(file, kind, expectFailure = false)((_, _) => ())

      case "neg" =>
        runTestCommon(file, kind, expectFailure = true)((logFile, outDir) => {
          // compare log file to check file
          val fileBase = basename(file.getName)
          val dir      = file.getParentFile

          diffCheck(
            // diff is contents of logFile
            if (!existsCheckFile(dir, fileBase, kind)) file2String(logFile)
            else compareOutput(dir, fileBase, kind, logFile)
          )
        })

      case "run" | "jvm" =>
        runJvmTest(file, kind)

      case "buildmanager" =>
        val logFile = createLogFile(file, kind)
        if (!fileManager.failed || logFile.canRead) {
          val swr = new StringWriter
          val wr = new PrintWriter(swr)
          succeeded = true; diff = ""
          printInfoStart(file, wr)
          val (outDir, testFile, changesDir, fileBase) =

          if (!file.isDirectory) {
            succeeded = false
            (null, null, null, null)
          } else {
            val fileBase: String = basename(file.getName)
            NestUI.verbose(this+" running test "+fileBase)
            val outDir = createOutputDir(file, fileBase, kind)
            if (!outDir.exists) outDir.mkdir()
            val testFile = new File(file, fileBase + ".test")
            val changesDir = new File(file, fileBase + ".changes")
            if (changesDir.isFile || !testFile.isFile) {
              // if changes exists then it has to be a dir
              if (!testFile.isFile) NestUI.verbose("invalid build manager test file")
              if (changesDir.isFile) NestUI.verbose("invalid build manager changes directory")
              succeeded = false
              (null, null, null, null)
            } else {
              copyTestFiles(file, outDir)
              NestUI.verbose("outDir:  "+outDir)
              NestUI.verbose("logFile: "+logFile)
              (outDir, testFile, changesDir, fileBase)
            }
          }

          if (succeeded) {
            // Pre-conditions satisfied

            try {
              val sourcepath = outDir.getAbsolutePath+File.separator

              // configure input/output files
              val logWriter = new PrintStream(new FileOutputStream(logFile))
              val testReader = new BufferedReader(new FileReader(testFile))
              val logConsoleWriter = new PrintWriter(logWriter)

              // create proper settings for the compiler
              val settings = new Settings(error)
              settings.outdir.value = outDir.getCanonicalFile.getAbsolutePath
              settings.sourcepath.value = sourcepath
              settings.classpath.value = fileManager.CLASSPATH
              settings.Ybuildmanagerdebug.value = true

              // simulate Build Manager loop
              val prompt = "builder > "
              reporter = new ConsoleReporter(settings, scala.Console.in, logConsoleWriter)
              val bM: BuildManager =
                  new RefinedBuildManager(settings) {
                    override protected def newCompiler(settings: Settings) =
                        new BuilderGlobal(settings, reporter)
                  }

              val testCompile = (line: String) => {
                NestUI.verbose("compiling " + line)
                val args = (line split ' ').toList
                val command = new CompilerCommand(args, settings)
                bM.update(filesToSet(settings.sourcepath.value, command.files), Set.empty)
                !reporter.hasErrors
              }

              val updateFiles = (line: String) => {
                NestUI.verbose("updating " + line)
                val res =
                  ((line split ' ').toList).forall(u => {
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
                  })
                if (!res)
                  NestUI.verbose("updating failed")
                else
                  NestUI.verbose("updating succeeded")
                res
              }

              def loop() {
                val command = testReader.readLine()
                if ((command ne null) && command.length() > 0) {
                  val commandResult = command match {
                    case s if (s.startsWith(">>update "))   =>
                      updateFiles(s.stripPrefix(">>update "))
                    case s if (s.startsWith(">>compile "))  =>
                      val files = s.stripPrefix(">>compile ")
                      logWriter.println(prompt + files)
                      testCompile(files) // In the end, it can finish with an error
                    case _                                  =>
                      NestUI.verbose("wrong command in test file: " + command)
                      false
                    }

                  if (commandResult) loop()

                } else {
                  NestUI.verbose("finished")
                  succeeded = true
                }
              }

              withOutputRedirected(logWriter) {
                loop()
                testReader.close()
              }
              fileManager.mapFile(logFile, "tmp", file, _.replace(sourcepath, "").
                      replaceAll(java.util.regex.Matcher.quoteReplacement("\\"), "/"))

              diffCheck(compareOutput(file, fileBase, kind, logFile))
            }
            LogContext(logFile, Some((swr, wr)))
          } else
            LogContext(logFile, None)
        } else
          LogContext(logFile, None)

      case "res" => {
          // when option "--failed" is provided
          // execute test only if log file is present
          // (which means it failed before)

          //val (logFileOut, logFileErr) = createLogFiles(file, kind)
          val logFile = createLogFile(file, kind)
          if (!fileManager.failed || logFile.canRead) {
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

            try {

            val sourcedir  = logFile.getParentFile.getCanonicalFile
            val sourcepath = sourcedir.getAbsolutePath+File.separator
            NestUI.verbose("sourcepath: "+sourcepath)

            val argString =
              "-d "+outDir.getCanonicalFile.getAbsolutePath+
              " -Xresident"+
              " -sourcepath "+sourcepath
            val argList = argString split ' ' toList

            // configure input/output files
            val logOut    = new FileOutputStream(logFile)
            val logWriter = new PrintStream(logOut)
            val resReader = new BufferedReader(new FileReader(resFile))
            val logConsoleWriter = new PrintWriter(new OutputStreamWriter(logOut))

            // create compiler
            val settings = new Settings(error)
            settings.sourcepath.value = sourcepath
            settings.classpath.value = fileManager.CLASSPATH
            reporter = new ConsoleReporter(settings, scala.Console.in, logConsoleWriter)
            val command = new CompilerCommand(argList, settings)
            object compiler extends Global(command.settings, reporter)

            // simulate resident compiler loop
            val prompt = "\nnsc> "

            val resCompile = (line: String) => {
              NestUI.verbose("compiling "+line)
              val cmdArgs = (line split ' ').toList map (fs => new File(dir, fs).getAbsolutePath)
              NestUI.verbose("cmdArgs: "+cmdArgs)
              val sett = new Settings(error)
              sett.sourcepath.value = sourcepath
              val command = new CompilerCommand(cmdArgs, sett)
              (new compiler.Run) compile command.files
            }

            def loop(action: (String) => Unit) {
              logWriter.print(prompt)
              val line = resReader.readLine()
              if ((line ne null) && line.length() > 0) {
/*
                val parent = self
                self.trapExit = true
                val child = link {
                  action(line)
                }

                receiveWithin(fileManager.timeout.toLong) {
                  case TIMEOUT =>
                    NestUI.verbose("action timed out")
                    false
                  case Exit(from, reason) if from == child => reason match {
                    case 'normal => // do nothing
                    case t: Throwable =>
                      NestUI.verbose("while invoking compiler:")
                      NestUI.verbose("caught "+t)
                      t.printStackTrace
                      if (t.getCause != null)
                        t.getCause.printStackTrace
                      false
                  }
                }
*/
                action(line)
                loop(action)
              }
            }

            withOutputRedirected(logWriter) {
              loop(resCompile)
              resReader.close()
            }

            def replaceSlashes(s: String): String = {
                val path = dir.getAbsolutePath+File.separator
                // find `path` in `line`
                val index = s.indexOf(path)
                val line =
                  if (index != -1)
                    s.substring(0, index) + s.substring(index + path.length, s.length)
                  else s
                line.replace('\\', '/')
            }

            fileManager.mapFile(logFile, "tmp", dir, replaceSlashes)
            diffCheck(compareOutput(dir, fileBase, kind, logFile))

            } catch {
              case e: Exception =>
	        e.printStackTrace()
                succeeded = false
            }

            LogContext(logFile, Some((swr, wr)))
          } else
            LogContext(logFile, None)
        }

      case "shootout" => {
          // when option "--failed" is provided
          // execute test only if log file is present
          // (which means it failed before)
          val logFile = createLogFile(file, kind)
          if (!fileManager.failed || logFile.canRead) {
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
              if (!compileMgr.shouldCompile(List(testFile), kind, logFile)) {
                NestUI.verbose("compilation of "+file+" failed\n")
                succeeded = false
              } else {
                NestUI.verbose("compilation of "+testFile+"succeeded")
                // -------- run test --------

                //TODO: detect whether we have to use Runtime.exec
                // val useRuntime = true
                //
                // if (useRuntime)
                //   execTest(outDir, logFile, fileBase)
                // else
                //   execTestObjectRunner(file, outDir, logFile)

                execTest(outDir, logFile, fileBase)

                NestUI.verbose(this+" finished running "+fileBase)
              } // successful compile
            } catch { // *catch-all*
              case e: Exception =>
                NestUI.verbose("caught "+e)
                succeeded = false
            }

            diffCheck(compareOutput(dir, fileBase, kind, logFile))

            LogContext(logFile, Some((swr, wr)))
          } else
            LogContext(logFile, None)
        }

      case "scalap" => {

        runInContext(file, kind, (logFile: File, outDir: File) => {
          val sourceDir = file.getParentFile
          val sourceDirName = sourceDir.getName

          // 1. Find file with result text
          val results = sourceDir.listFiles(new FilenameFilter {
            def accept(dir: File, name: String) = name == "result.test"
          })

          if (results.length != 1) {
            NestUI.verbose("Result file not found in directory " + sourceDirName + " \n")
          } else {
            val resFile = results(0)
            // 2. Compile source file
            if (!compileMgr.shouldCompile(outDir, List(file), kind, logFile)) {
              NestUI.verbose("compilerMgr failed to compile %s to %s".format(file, outDir))
              succeeded = false
            } else {

              // 3. Decompile file and compare results
              val isPackageObject = sourceDir.getName.startsWith("package")
              val className = sourceDirName.capitalize + (if (!isPackageObject) "" else ".package")
              val url = outDir.toURI.toURL
              val loader = new URLClassLoader(Array(url), getClass.getClassLoader)
              val clazz = loader.loadClass(className)

              val byteCode = ByteCode.forClass(clazz)
              val result = scala.tools.scalap.Main.decompileScala(byteCode.bytes, isPackageObject)

              try {
                val fstream = new FileWriter(logFile);
                val out = new BufferedWriter(fstream);
                out.write(result)
                out.close();
              } catch {
                case e: IOException => NestUI.verbose(e.getMessage()); succeeded = false
              }

              diffCheck(fileManager.compareFiles(logFile, resFile))
            }
          }
        })
      }

      case "script" => {
          // when option "--failed" is provided
          // execute test only if log file is present
          // (which means it failed before)
          val logFile = createLogFile(file, kind)
          if (!fileManager.failed || logFile.canRead) {
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
              val cmdString =
                if (isWin) {
                  val batchFile = new File(file.getParentFile, fileBase+".bat")
                  NestUI.verbose("batchFile: "+batchFile)
                  batchFile.getAbsolutePath
                }
                else file.getAbsolutePath
              val proc = Runtime.getRuntime.exec(cmdString+argString)
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

              diffCheck(compareOutput(file.getParentFile, fileBase, kind, logFile))
            } catch { // *catch-all*
              case e: Exception =>
                NestUI.verbose("caught "+e)
                succeeded = false
            }

            LogContext(logFile, Some((swr, wr)))
          } else
            LogContext(logFile, None)
      }
    }

    def reportAll(results: ImmMap[String, Int], cont: ImmMap[String, Int] => Unit) {
      // NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
      // NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
      timer.cancel()
      cont(results)
    }

    def reportResult(state: Int, logFile: Option[LogFile], writers: Option[(StringWriter, PrintWriter)]) {
      val good = (state == 0)
      if (!good) {
        errors += 1
        NestUI.verbose("incremented errors: "+errors)
      }

      try {
        // delete log file only if test was successful
        if (good && !logFile.isEmpty && !isPartestDebug)
          logFile.get.toDelete = true

        writers match {
          case Some((swr, wr)) =>
            if (state == 2)
              printInfoTimeout(wr)
            else
              printInfoEnd(good, wr)
            wr.flush()
            swr.flush()
            NestUI.normal(swr.toString)
            if (state == 1 && fileManager.showDiff && diff != "")
              NestUI.normal(diff)
            if (state == 1 && fileManager.showLog)
              showLog(logFile.get)
          case None =>
        }
      } catch {
        case npe: NullPointerException =>
      }
    }

    val numFiles = files.size
    if (numFiles == 0)
      reportAll(ImmMap(), topcont)

    // maps canonical file names to the test result (0: OK, 1: FAILED, 2: TIMOUT)
    var status = new HashMap[String, Int]

    var fileCnt = 1
    Actor.loopWhile(fileCnt <= numFiles) {
      val parent = self

      actor {
        val testFile = files(fileCnt-1)

        val ontimeout = new TimerTask {
          def run() = parent ! Timeout(testFile)
        }
        timer.schedule(ontimeout, fileManager.timeout.toLong)

        val context = try {
          processSingleFile(testFile)
        } catch {
          case t: Throwable =>
            NestUI.verbose("while invoking compiler ("+files+"):")
            NestUI.verbose("caught "+t)
            t.printStackTrace
            if (t.getCause != null)
              t.getCause.printStackTrace
            LogContext(null, None)
        }
        parent ! Result(testFile, context)
      }

      react {
        case res: TestResult =>
          val path = res.file.getCanonicalPath
          status.get(path) match {
            case Some(stat) => // ignore message
            case None =>
              res match {
                case Timeout(_) =>
                  status = status + (path -> 2)
                  val swr = new StringWriter
                  val wr = new PrintWriter(swr)
                  printInfoStart(res.file, wr)
                  succeeded = false
                  reportResult(2, None, Some((swr, wr)))
                case Result(_, logs) =>
                  status = status + (path -> (if (succeeded) 0 else 1))
                  reportResult(
                    if (succeeded) 0 else 1,
                    if (logs != null) Some(logs.file) else None,
                    if (logs != null) logs.writers else None)
              }
              if (fileCnt == numFiles)
                reportAll(status, topcont)
              fileCnt += 1
          }
      }
    }
  }

  private def withOutputRedirected(out: PrintStream)(func: => Unit) {
    val oldStdOut = System.out
    val oldStdErr = System.err

    try {
      System.setOut(out)
      System.setErr(out)
      func
      out.flush()
      out.close()
    } finally {
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
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

  def showLog(logFile: File) {
    try {
      val logReader = new BufferedReader(new FileReader(logFile))
      val strWriter = new StringWriter
      val logWriter = new PrintWriter(strWriter, true)
      val logAppender = new StreamAppender(logReader, logWriter)
      logAppender.run()
      logReader.close()
      val log = strWriter.toString
      NestUI.normal(log)
    } catch {
      case fnfe: java.io.FileNotFoundException =>
        NestUI.failure("Couldn't open log file \""+logFile+"\".")
    }
  }
}
