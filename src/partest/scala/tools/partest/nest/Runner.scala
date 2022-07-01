/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest
package nest

import java.io.{Console => _, _}
import java.lang.reflect.InvocationTargetException
import java.nio.charset.Charset
import java.nio.file.{Files, StandardOpenOption}

import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.reflect.internal.FatalError
import scala.reflect.internal.util.ScalaClassLoader
import scala.sys.process.{Process, ProcessLogger}
import scala.tools.nsc.Properties.{isWin, propOrEmpty}
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.stackTraceString
import ClassPath.join
import TestState.{Crash, Fail, Pass, Skip, Updated}
import FileManager.{compareContents, joinPaths, withTempFile}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.util.{Failure, Success, Try, Using}
import scala.util.chaining._
import scala.util.control.ControlThrowable

/** pos/t1234.scala or pos/t1234 if dir */
case class TestInfo(testFile: File) {
  /** pos/t1234.scala */
  val testIdent: String = testFile.testIdent

  /** pos */
  val kind: String = parentFile.getName

  // inputs

  /** pos/t1234.check */
  val checkFile: File = testFile.changeExtension("check")

  // outputs

  /** pos/t1234-pos.log */
  val logFile: File = new File(parentFile, s"$fileBase-$kind.log")

  /** pos/t1234-pos.obj */
  val outFile: File = logFile.changeExtension("obj")

  // enclosing dir
  def parentFile: File = testFile.getParentFile

  // test file name without extension
  def fileBase: String = basename(testFile.getName)
}

/** Run a single test. */
class Runner(val testInfo: TestInfo, val suiteRunner: AbstractRunner) {
  private val stopwatch = new Stopwatch()

  import testInfo._
  import suiteRunner.{fileManager => fm, _}
  val fileManager = fm

  import fileManager._

  private val _transcript = new TestTranscript

  def pushTranscript(msg: String) = _transcript.add(msg)

  lazy val outDir = { outFile.mkdirs() ; outFile }

  // if there is a checkfile, log message for diff; otherwise log stack trace for post-mortem
  def crashHandler: PartialFunction[Throwable, TestState] = {
    case _: InterruptedException => genTimeout()
    case t: FatalError if checkFile.canRead =>
      logFile.appendAll(s"fatal error: ${t.getMessage}")
      genCrash(t)
    case t: Throwable =>
      if (!suiteRunner.terse) System.err.println(s"Crashed running test $testIdent: " + t)
      logFile.appendAll(stackTraceString(t))
      genCrash(t)
  }

  def genPass(): TestState        = Pass(testFile)
  def genFail(reason: String)     = Fail(testFile, reason, transcript.toArray)
  def genResult(b: Boolean)       = if (b) genPass() else genFail("predicate failed")
  def genSkip(reason: String)     = Skip(testFile, reason)
  def genTimeout()                = Fail(testFile, "timed out", transcript.toArray)
  def genCrash(caught: Throwable) = Crash(testFile, caught, transcript.toArray)
  def genUpdated()                = Updated(testFile)

  private def workerError(msg: String): Unit = System.err.println("Error: " + msg)

  def javac(files: List[File]): TestState = {
    // compile using command-line javac compiler
    val args = Seq(
      javacCmdPath,
      "-d",
      outDir.getAbsolutePath,
      "-classpath",
      joinPaths(outDir :: testClassPath),
      "-J-Duser.language=en",
      "-J-Duser.country=US"
    ) ++ (toolArgsFor(files)("javac")
    ) ++ (files.map(_.getAbsolutePath)
    )

    pushTranscript(args mkString " ")
    if (runCommand(args, logFile)) genPass() else {
      genFail("java compilation failed")
    }
  }

  /** Evaluate an action body and judge whether it passed. */
  def nextTestAction[T](body: => T)(eval: PartialFunction[T, TestState]): TestState = eval.applyOrElse(body, (_: T) => genPass())
  /** If the action does not result in true, fail the action. */
  def nextTestActionExpectTrue(reason: String, body: => Boolean): TestState = nextTestAction(body) { case false => genFail(reason) }
  /** Fail the action. */
  def nextTestActionFailing(reason: String): TestState = nextTestActionExpectTrue(reason, false)

  private def assembleTestCommand(outDir: File, javaopts: List[String]): List[String] = {
    if (javaopts.nonEmpty)
      suiteRunner.verbose(s"Using java options: '${javaopts.mkString(",")}'")

    val propertyOpts = propertyOptions(fork = true).map { case (k, v) => s"-D$k=$v" }

    val classpath = joinPaths(extraClasspath ++ testClassPath)

    // `javaopts` last; for repeated arguments, the last one wins
    javaCmdPath +: (
      (suiteRunner.javaOpts.split(' ') ++ extraJavaOptions ++ javaopts).filter(_ != "").toList ++ Seq(
        "-classpath",
        join(outDir.toString, classpath)
      ) ++ propertyOpts ++ Seq(
        "scala.tools.nsc.MainGenericRunner",
        "-usejavacp",
        "Test",
        "jvm"
      )
    )
  }

  def propertyOptions(fork: Boolean): List[(String, String)] = {
    val testFullPath = testFile.getAbsolutePath
    val extras =   if (suiteRunner.debug) List("partest.debug" -> "true") else Nil
    val immutablePropsToCheck = List[(String, String)](
      "file.encoding" -> "UTF-8",
      "user.language" -> "en",
      "user.country" -> "US"
    )
    val immutablePropsForkOnly = List[(String, String)](
      "java.library.path" -> logFile.getParentFile.getAbsolutePath,
    )
    val shared = List(
      "partest.output" -> ("" + outDir.getAbsolutePath),
      "partest.lib" -> ("" + libraryUnderTest.jfile.getAbsolutePath),
      "partest.reflect" -> ("" + reflectUnderTest.jfile.getAbsolutePath),
      "partest.comp" -> ("" + compilerUnderTest.jfile.getAbsolutePath),
      "partest.cwd" -> ("" + outDir.getParent),
      "partest.test-path" -> ("" + testFullPath),
      "partest.testname" -> ("" + fileBase),
      "javacmd" -> ("" + javaCmdPath),
      "javaccmd" -> ("" + javacCmdPath),
    ) ++ extras
    if (fork) {
      immutablePropsToCheck ++ immutablePropsForkOnly ++ shared
    } else {
      for ((k, requiredValue) <- immutablePropsToCheck) {
        val actual = System.getProperty(k)
        assert(actual == requiredValue, s"Unable to run test without forking as the current JVM has an incorrect system property. For $k, found $actual, required $requiredValue")
      }
      shared
    }
  }

  /** Runs command redirecting standard out and
   *  error out to output file.
   */
  protected def runCommand(args: Seq[String], outFile: File): Boolean = {
    val nonzero = 17     // rounding down from 17.3
    //(Process(args) #> outFile !) == 0 or (Process(args) ! pl) == 0
    Using.resource(ProcessLogger(outFile)) { pl =>
      def run: Int = {
        val p =
          Try(Process(args).run(pl)) match {
            case Failure(e) => outFile.appendAll(stackTraceString(e)) ; return -1
            case Success(v) => v
          }
        try p.exitValue()
        catch {
          case e: InterruptedException =>
            suiteRunner.verbose(s"Interrupted waiting for command to finish (${args mkString " "})")
            p.destroy()
            nonzero
          case t: Throwable =>
            suiteRunner.verbose(s"Exception waiting for command to finish: $t (${args mkString " "})")
            p.destroy()
            throw t
        }
      }
      pl.buffer(run) == 0
    }
  }

  private def execTest(outDir: File, logFile: File, javaopts: List[String]): TestState = {
    val cmd = assembleTestCommand(outDir, javaopts)

    pushTranscript((cmd mkString s" \\$EOL  ") + " > " + logFile.getName)
    nextTestAction(runCommand(cmd, logFile)) {
      case false =>
        _transcript append EOL + logFile.fileContents
        genFail("non-zero exit code")
    }
  }

  def execTestInProcess(classesDir: File, log: File): TestState = {
    stopwatch.pause()
    suiteRunner.synchronized {
      stopwatch.start()
      def run(): Unit = {
        StreamCapture.withExtraProperties(propertyOptions(fork = false).toMap) {
          try {
            val out = Files.newOutputStream(log.toPath, StandardOpenOption.APPEND)
            try {
              val loader = new URLClassLoader(classesDir.toURI.toURL :: Nil, getClass.getClassLoader)
              StreamCapture.capturingOutErr(out) {
                val cls = loader.loadClass("Test")
                val main = cls.getDeclaredMethod("main", classOf[Array[String]])
                try {
                  main.invoke(null, Array[String]("jvm"))
                } catch {
                  case ite: InvocationTargetException => throw ite.getCause
                }
              }
            }  finally {
              out.close()
            }
          } catch {
            case t: ControlThrowable => throw t
            case t: Throwable =>
              // We'll let the checkfile diffing report this failure
              Files.write(log.toPath, stackTraceString(t).getBytes(Charset.defaultCharset()), StandardOpenOption.APPEND)
          }
        }
      }

      pushTranscript(s"<in process execution of $testIdent> > ${logFile.getName}")

      @nowarn("cat=deprecation")  // JDK 17 deprecates SecurityManager, so TrapExit is deprecated too
      val trapExit = TrapExit

      trapExit(() => run()) match {
        case Left((status, throwable)) if status != 0 =>
          genFail("non-zero exit code")
        case _ =>
          genPass()
      }
    }
  }

  override def toString = s"Test($testIdent)"

  def fail(what: Any) = {
    suiteRunner.verbose("scalac: compilation of "+what+" failed\n")
    false
  }

  /** Filter the check file for conditional blocks.
   *  The check file can contain lines of the form:
   *  `#partest java7`
   *  where the line contains a conventional flag name.
   *  If the flag tests true, succeeding lines are retained
   *  (removed on false) until the next #partest flag.
   *  A missing flag evaluates the same as true.
   */
  def filteredCheck: Seq[String] = {
    import scala.util.Properties.{javaSpecVersion, isAvian, isWin}
    import scala.tools.nsc.settings.ScalaVersion
    // use lines in block with this label?
    def retainOn(expr0: String) = {
      val expr = expr0.trim
      def flagWasSet(f: String) = {
        val allArgs = suiteRunner.scalacExtraArgs ++ suiteRunner.scalacOpts.split(' ')
        allArgs contains f
      }
      val (invert, token) = if (expr startsWith "!") (true, expr drop 1) else (false, expr)
      val javaN = raw"java(\d+)(\+)?".r
      val cond = token.trim match {
        case javaN(v, up) =>
          val required = ScalaVersion(if (v.toInt <= 8) s"1.$v" else v)
          val current  = ScalaVersion(javaSpecVersion)
          if (up != null) current >= required else current == required
        case "avian"  => isAvian
        case "isWin"  => isWin
        case "true"   => true
        case "-optimise" | "-optimize"
                      => flagWasSet("-optimise") || flagWasSet("-optimize")
        case flag if flag startsWith "-"
                      => flagWasSet(flag)
        case rest     => rest.isEmpty
      }
      if (invert) !cond else cond
    }
    val prefix = "#partest"
    val b = new ListBuffer[String]()
    var on = true
    for (line <- checkFile.fileContents.linesIfNonEmpty) {
      if (line startsWith prefix) {
        on = retainOn(line stripPrefix prefix)
      } else if (on) {
        b += line
      }
    }
    b.toList
  }

  // diff logfile checkfile
  def currentDiff = {
    val logged = logFile.fileContents.linesIfNonEmpty.toList
    val (checked, checkname) = if (checkFile.canRead) (filteredCheck, checkFile.getName) else (Nil, "empty")
    compareContents(original = checked, revised = logged, originalName = checkname, revisedName = logFile.getName)
  }

  val gitRunner = List("/usr/local/bin/git", "/usr/bin/git") map (f => new java.io.File(f)) find (_.canRead)
  val gitDiffOptions = "--ignore-space-at-eol --no-index " + propOrEmpty("partest.git_diff_options")
    // --color=always --word-diff

  def gitDiff(f1: File, f2: File): Option[String] = {
    try gitRunner map { git =>
      val cmd  = s"$git diff $gitDiffOptions $f1 $f2"
      val diff = Process(cmd).lazyLines_!.drop(4).map(_ + "\n").mkString

      "\n" + diff
    }
    catch { case t: Exception => None }
  }

  /** Normalize the log output by applying test-specific filters
   *  and fixing filesystem-specific paths.
   *
   *  Line filters are picked up from `filter: pattern` at the top of sources.
   *  The filtered line is detected with a simple "contains" test,
   *  and yes, "filter" means "filter out" in this context.
   *
   *  File paths are detected using the absolute path of the test root.
   *  A string that looks like a file path is normalized by replacing
   *  the leading segments (the root) with "\$ROOT" and by replacing
   *  any Windows backslashes with the one true file separator char.
   */
  def normalizeLog(): Unit = {
    import scala.util.matching.Regex

    // Apply judiciously; there are line comments in the "stub implementations" error output.
    val slashes    = """[/\\]+""".r
    def squashSlashes(s: String) = slashes.replaceAllIn(s, "/")

    // this string identifies a path and is also snipped from log output.
    val elided     = parentFile.getAbsolutePath

    // something to mark the elision in the log file (disabled)
    val ellipsis   = "" //".../"    // using * looks like a comment

    // no spaces in test file paths below root, because otherwise how to detect end of path string?
    val pathFinder = raw"""(?i)\Q${elided}${File.separator}\E([\${File.separator}\S]*)""".r
    def canonicalize(s: String): String =
      pathFinder.replaceAllIn(s, m => Regex.quoteReplacement(ellipsis + squashSlashes(m group 1)))

    def masters    = {
      val files = List(new File(parentFile, "filters"), new File(suiteRunner.pathSettings.srcDir.path, "filters"))
      files.filter(_.exists).flatMap(_.fileLines).map(_.trim).filter(s => !(s startsWith "#"))
    }
    val filters    = toolArgs("filter", split = false) ++ masters
    val elisions   = ListBuffer[String]()
    //def lineFilter(s: String): Boolean  = !(filters exists (s contains _))
    def lineFilter(s: String): Boolean  = (
      filters map (_.r) forall { r =>
        val res = (r findFirstIn s).isEmpty
        if (!res) elisions += s
        res
      }
    )

    logFile.mapInPlace(canonicalize)(lineFilter)
    if (suiteRunner.verbose && elisions.nonEmpty) {
      import suiteRunner.log._
      val emdash = bold(yellow("--"))
      pushTranscript(s"filtering ${logFile.getName}$EOL${elisions.mkString(emdash, EOL + emdash, EOL)}")
    }
  }

  def diffIsOk: TestState = {
    // always normalize the log first
    normalizeLog()
    pushTranscript(s"diff $checkFile $logFile")
    currentDiff match {
      case "" => genPass()
      case diff if config.optUpdateCheck =>
        suiteRunner.verbose("Updating checkfile " + checkFile)
        checkFile.writeAll(logFile.fileContents)
        genUpdated()
      case diff =>
        // Get a word-highlighted diff from git if we can find it
        val bestDiff =
          if (!checkFile.canRead) diff
          else
            gitRunner.flatMap(_ => withTempFile(outDir, fileBase, filteredCheck)(f =>
                gitDiff(f, logFile))).getOrElse(diff)
        _transcript append bestDiff
        genFail("output differs")
    }
  }

  /** 1. Creates log file and output directory.
   *  2. Runs script function, providing log file and output directory as arguments.
   *     2b. or, just run the script without context and return a new context
   */
  def runInContext(body: => TestState): TestState = {
    body
  }

  /** Grouped files in group order, and lex order within each group. */
  def groupedFiles(sources: List[File]): List[List[File]] = (
    if (sources.sizeIs > 1) {
      val grouped = sources groupBy (_.group)
      grouped.keys.toList.sorted map (k => grouped(k) sortBy (_.getName))
    }
    else List(sources)
  )

  /** Source files for the given test file. */
  def sources(file: File): List[File] = if (file.isDirectory) file.listFiles.toList.filter(_.isJavaOrScala) else List(file)

  def newCompiler = new DirectCompiler(this)

  def attemptCompile(sources: List[File]): TestState = {
    val badflags = (testFile :: (if (testFile.isDirectory) sources else Nil)).map(_.changeExtension("flags")).find(_.exists)
    if (badflags.isDefined) genFail(s"unexpected flags file ${badflags.get} (use source comment // scalac: -Werror)")
    else
      newCompiler.compile(flagsForCompilation(sources), sources).tap { state =>
        if (!state.isOk) _transcript.append("\n" + logFile.fileContents)
      }
  }

  // all sources in a round may contribute flags via // scalac: -flags
  def flagsForCompilation(sources: List[File]): List[String] = {
    val perFile = toolArgsFor(sources)("scalac")
    if (parentFile.getParentFile.getName == "macro-annot") "-Ymacro-annotations" :: perFile
    else perFile
  }

  // inspect sources for tool args
  def toolArgs(tool: String, split: Boolean = true): List[String] =
    toolArgsFor(sources(testFile))(tool, split)

  // inspect given files for tool args of the form `tool: args`
  // if args string ends in close comment, drop the `*` `/`
  // if split, parse the args string as command line.
  //
  def toolArgsFor(files: List[File])(tool: String, split: Boolean = true): List[String] = {
    def argsFor(f: File): List[String] = {
      import scala.jdk.OptionConverters._
      import scala.tools.cmd.CommandLineParser.tokenize
      val max  = 10
      val tag  = s"$tool:"
      val endc = "*" + "/"    // be forgiving of /* scalac: ... */
      def stripped(s: String) = s.substring(s.indexOf(tag) + tag.length).stripSuffix(endc)
      def argsplitter(s: String) = if (split) tokenize(s) else List(s.trim())
      val args = Using.resource(Files.lines(f.toPath, codec.charSet))(
        _.limit(max).filter(_.contains(tag)).map(stripped).findAny.toScala
      )
      args.map(argsplitter).getOrElse(Nil)
    }
    files.flatMap(argsFor)
  }

  abstract class CompileRound {
    def fs: List[File]
    def result: TestState
    def description: String

    def fsString = fs map (_.toString stripPrefix parentFile.toString + "/") mkString " "
    def isOk = result.isOk
    def mkScalacString(): String = s"""scalac $fsString"""
    override def toString = description + ( if (result.isOk) "" else "\n" + result.status )
  }
  case class OnlyJava(fs: List[File]) extends CompileRound {
    def description = s"""javac $fsString"""
    lazy val result = { pushTranscript(description) ; javac(fs) }
  }
  case class OnlyScala(fs: List[File]) extends CompileRound {
    def description = mkScalacString()
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }
  case class ScalaAndJava(fs: List[File]) extends CompileRound {
    def description = mkScalacString()
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }
  case class SkipRound(fs: List[File], state: TestState) extends CompileRound {
    def description: String = state.status
    lazy val result = { pushTranscript(description); state }
  }

  def compilationRounds(file: File): List[CompileRound] = {
    import scala.util.Properties.javaSpecVersion
    val Range = """(\d+)(?:(\+)|(?: *\- *(\d+)))?""".r
    lazy val currentJavaVersion = javaSpecVersion.stripPrefix("1.").toInt
    val allFiles = sources(file)
    val skipStates = toolArgsFor(allFiles)("javaVersion", split = false).flatMap({
      case v @ Range(from, plus, to) =>
        val ok =
          if (plus == null)
            if (to == null) currentJavaVersion == from.toInt
            else from.toInt <= currentJavaVersion && currentJavaVersion <= to.toInt
          else
            currentJavaVersion >= from.toInt
        if (ok) None
        else Some(genSkip(s"skipped on Java $javaSpecVersion, only running on $v"))
      case v =>
        Some(genFail(s"invalid javaVersion range in test comment: $v"))
    })
    skipStates.headOption match {
      case Some(state) => List(SkipRound(List(file), state))
      case _ => groupedFiles(allFiles).flatMap(mixedCompileGroup)
    }
  }

  def mixedCompileGroup(allFiles: List[File]): List[CompileRound] = {
    val (scalaFiles, javaFiles) = allFiles partition (_.isScala)
    val round1                  = if (scalaFiles.isEmpty) None else Some(ScalaAndJava(allFiles))
    val round2                  = if (javaFiles.isEmpty) None else Some(OnlyJava(javaFiles))

    List(round1, round2).flatten
  }

  def runPosTest(): TestState =
    if (checkFile.exists) genFail("unexpected check file for pos test (use -Werror with neg test to verify warnings)")
    else runTestCommon()

  def runNegTest(): TestState = runInContext {
    // pass if it checks and didn't crash the compiler
    // or, OK, we'll let you crash the compiler with a FatalError if you supply a check file
    def checked(r: CompileRound) = r.result match {
      case s: Skip => s
      case crash @ Crash(_, t, _) if !checkFile.canRead || !t.isInstanceOf[FatalError] => crash
      case _ => diffIsOk
    }

    compilationRounds(testFile).find(r => !r.result.isOk || r.result.isSkipped).map(checked).getOrElse(genFail("expected compilation failure"))
  }

  // run compilation until failure, evaluate `andAlso` on success
  def runTestCommon(andAlso: => TestState = genPass()): TestState = runInContext {
    // DirectCompiler already says compilation failed
    val res = compilationRounds(testFile).find(r => !r.result.isOk || r.result.isSkipped).map(_.result).getOrElse(genPass())
    res andAlso andAlso
  }

  def extraClasspath = kind match {
    case "specialized"  => List(suiteRunner.pathSettings.srcSpecLib.fold(sys.error, identity))
    case _              => Nil
  }
  def extraJavaOptions = kind match {
    case "instrumented" => ("-javaagent:"+agentLib).split(' ')
    case _              => Array.empty[String]
  }

  def runResidentTest(): TestState = {
    // simulate resident compiler loop
    val prompt = "\nnsc> "

    suiteRunner.verbose(s"$this running test $fileBase")
    val dir = parentFile
    val resFile = new File(dir, fileBase + ".res")

    // run compiler in resident mode
    // $SCALAC -d "$os_dstbase".obj -Xresident -sourcepath . "$@"
    val sourcedir  = logFile.getParentFile.getAbsoluteFile
    val sourcepath = sourcedir.getAbsolutePath+File.separator
    suiteRunner.verbose("sourcepath: "+sourcepath)

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
    settings.classpath.value = joinPaths(fileManager.testClassPath)
    val reporter = new ConsoleReporter(settings, scala.Console.in, logConsoleWriter)
    val command = new CompilerCommand(argList, settings)
    object compiler extends Global(command.settings, reporter)

    def resCompile(line: String): TestState = {
      // suiteRunner.verbose("compiling "+line)
      val cmdArgs = (line split ' ').toList map (fs => new File(dir, fs).getAbsolutePath)
      // suiteRunner.verbose("cmdArgs: "+cmdArgs)
      val sett = new Settings(workerError)
      sett.sourcepath.value = sourcepath
      val command = new CompilerCommand(cmdArgs, sett)
      // "scalac " + command.files.mkString(" ")
      pushTranscript("scalac " + command.files.mkString(" "))
      nextTestActionExpectTrue(
        "compilation failed",
        command.ok && {
          (new compiler.Run) compile command.files
          !reporter.hasErrors
        }
      )
    }
    def loop(): TestState = {
      logWriter.print(prompt)
      resReader.readLine() match {
        case null | ""  => logWriter.close() ; genPass()
        case line       => resCompile(line) andAlso loop()
      }
    }
    // res/t687.res depends on ignoring its compilation failure
    // and just looking at the diff, so I made them all do that
    // because this is long enough.
    /*val res =*/ Output.withRedirected(logWriter)(try loop() finally resReader.close())

    /*res andAlso*/ diffIsOk
  }

  def run(): (TestState, Long) = {
    // javac runner, for one, would merely append to an existing log file, so just delete it before we start
    logFile.delete()
    stopwatch.start()

    val state =  kind match {
      case "pos"          => runPosTest()
      case "neg"          => runNegTest()
      case "res"          => runResidentTest()
      case "scalap"       => runScalapTest()
      case "script"       => runScriptTest()
      case k if k.endsWith("-neg") => runNegTest()
      case _              => runRunTest()
    }
    (state, stopwatch.stop())
  }

  private def runRunTest(): TestState = {
    val javaopts = toolArgs("java")
    val execInProcess = PartestDefaults.execInProcess && javaopts.isEmpty && !Set("specialized", "instrumented").contains(testFile.getParentFile.getName)
    def exec() = if (execInProcess) execTestInProcess(outDir, logFile) else execTest(outDir, logFile, javaopts)
    def noexec() = genSkip("no-exec: tests compiled but not run")
    runTestCommon(if (suiteRunner.config.optNoExec) noexec() else exec().andAlso(diffIsOk))
  }

  private def decompileClass(clazz: Class[_], isPackageObject: Boolean): String = {
    import scala.tools.scalap
    import scalap.scalax.rules.scalasig.ByteCode

    scalap.Main.decompileScala(ByteCode.forClass(clazz).bytes, isPackageObject)
  }

  def runScalapTest(): TestState = runTestCommon {
    val isPackageObject = testFile.getName startsWith "package"
    val className       = testFile.getName.stripSuffix(".scala").capitalize + (if (!isPackageObject) "" else ".package")
    val loader          = ScalaClassLoader.fromURLs(List(outDir.toURI.toURL), this.getClass.getClassLoader)
    logFile writeAll decompileClass(loader loadClass className, isPackageObject)
    diffIsOk
  }

  def runScriptTest(): TestState = {
    import scala.sys.process._

    val args = testFile.changeExtension("args").fileContents
    val cmdFile = if (isWin) testFile changeExtension "bat" else testFile
    val succeeded = (((s"$cmdFile $args" #> logFile).!) == 0)

    val result = if (succeeded) genPass() else genFail(s"script $cmdFile failed to run")

    result andAlso diffIsOk
  }

  def cleanup(state: TestState): Unit = {
    if (state.isOk) logFile.delete()
    if (!suiteRunner.debug) Directory(outDir).deleteRecursively()
  }

  // Colorize prompts according to pass/fail
  def transcript: List[String] = {
    import suiteRunner.log._
    def pass(s: String) = bold(green("% ")) + s
    def fail(s: String) = bold(red("% ")) + s
    _transcript.toList match {
      case Nil  => Nil
      case xs   => (xs.init map pass) :+ fail(xs.last)
    }
  }
}

/** Loads `library.properties` from the jar. */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "partest"
  protected def pickJarBasedOn  = classOf[AbstractRunner]
}

case class TimeoutException(duration: Duration) extends RuntimeException

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

final class TestTranscript {
  private[this] val buf = ListBuffer[String]()

  def add(action: String): this.type = { buf += action ; this }
  def append(text: String): Unit = { val s = buf.last ; buf.dropRightInPlace(1) ; buf += (s + text) }
  def toList = buf.toList
}
