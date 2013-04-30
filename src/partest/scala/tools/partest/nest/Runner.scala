/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */
package scala.tools.partest
package nest

import java.io.{ Console => _, _ }
import java.net.URL
import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder, CharacterCodingException, CodingErrorAction => Action }
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.NANOSECONDS
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.io.Codec
import scala.sys.process.Process
import scala.tools.nsc.Properties.{ envOrElse, isWin, jdkHome, javaHome, propOrElse, propOrEmpty, setProp }
import scala.tools.nsc.{ Settings, CompilerCommand, Global }
import scala.tools.nsc.io.{ AbstractFile, PlainFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ Exceptional, ScalaClassLoader, stackTraceString }
import scala.tools.scalap.Main.decompileScala
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.util.{ Try, Success, Failure }
import ClassPath.{ join, split }
import PartestDefaults.{ javaCmd, javacCmd }
import TestState.{ Pass, Fail, Crash, Uninitialized, Updated }

trait PartestRunSettings {
  def gitPath: Path
  def reportPath: Path
  def logPath: Path

  def testPaths: List[Path]

  def gitDiffOptions: List[String]
  def extraScalacOptions: List[String]
  def extraJavaOptions: List[String]
}

class TestTranscript {
  import NestUI.color._
  private val buf = ListBuffer[String]()
  private def pass(s: String) = bold(green("% ")) + s
  private def fail(s: String) = bold(red("% ")) + s

  def add(action: String): this.type = { buf += action ; this }
  def append(text: String) { val s = buf.last ; buf.trimEnd(1) ; buf += (s + text) }

  // Colorize prompts according to pass/fail
  def fail: List[String] = buf.toList match {
    case Nil  => Nil
    case xs   => (xs.init map pass) :+ fail(xs.last)
  }
}

/** Run a single test. Rubber meets road. */
class Runner(val testFile: File, fileManager: FileManager, val testRunParams: TestRunParams) {
  import fileManager._

  // Override to true to have the outcome of this test displayed
  // whether it passes or not; in general only failures are reported,
  // except for a . per passing test to show progress.
  def isEnumeratedTest = false

  private var _lastState: TestState = null
  private var _transcript = new TestTranscript

  def lastState                   = if (_lastState == null) Uninitialized(testFile) else _lastState
  def setLastState(s: TestState)  = _lastState = s
  def transcript: List[String]    = _transcript.fail ++ logFile.fileLines
  def pushTranscript(msg: String) = _transcript add msg

  val parentFile = testFile.getParentFile
  val kind       = parentFile.getName
  val fileBase   = basename(testFile.getName)
  val logFile    = new File(parentFile, s"$fileBase-$kind.log")
  val outFile    = logFile changeExtension "obj"
  val checkFile  = testFile changeExtension "check"
  val flagsFile  = testFile changeExtension "flags"
  val testIdent  = testFile.testIdent // e.g. pos/t1234

  lazy val outDir = { outFile.mkdirs() ; outFile }

  type RanOneTest = (Boolean, LogContext)

  def showCrashInfo(t: Throwable) {
    System.err.println("Crashed running test $testIdent: " + t)
    if (!isPartestTerse)
      System.err.println(stackTraceString(t))
  }
  protected def crashHandler: PartialFunction[Throwable, TestState] = {
    case t: InterruptedException =>
      genTimeout()
    case t: Throwable =>
      showCrashInfo(t)
      logFile.appendAll(stackTraceString(t))
      genCrash(t)
  }

  def genPass()                   = Pass(testFile)
  def genFail(reason: String)     = Fail(testFile, reason, _transcript.fail)
  def genTimeout()                = Fail(testFile, "timed out", _transcript.fail)
  def genCrash(caught: Throwable) = Crash(testFile, caught, _transcript.fail)
  def genUpdated()                = Updated(testFile)

  def speclib = PathSettings.srcSpecLib.toString  // specialization lib
  def codelib = PathSettings.srcCodeLib.toString  // reify lib

  // Prepend to a classpath, but without incurring duplicate entries
  def prependTo(classpath: String, path: String): String = {
    val segments = ClassPath split classpath

    if (segments startsWith path) classpath
    else ClassPath.join(path :: segments distinct: _*)
  }

  def prependToJavaClasspath(path: String) {
    val jcp = sys.props.getOrElse("java.class.path", "")
    prependTo(jcp, path) match {
      case `jcp`  =>
      case cp     => sys.props("java.class.path") = cp
    }
  }
  def prependToClasspaths(s: Settings, path: String) {
    prependToJavaClasspath(path)
    val scp = s.classpath.value
    prependTo(scp, path) match {
      case `scp`  =>
      case cp     => s.classpath.value = cp
    }
  }

  private def workerError(msg: String): Unit = System.err.println("Error: " + msg)

  def javac(files: List[File]): TestState = {
    // compile using command-line javac compiler
    val args = Seq(
      javacCmd,
      "-d",
      outDir.getAbsolutePath,
      "-classpath",
      join(outDir.toString, CLASSPATH)
    ) ++ files.map(_.getAbsolutePath)

    pushTranscript(args mkString " ")
    val captured = StreamCapture(runCommand(args, logFile))
    if (captured.result) genPass() else {
      logFile appendAll captured.stderr
      genFail("java compilation failed")
    }
  }

  def testPrompt = kind match {
    case "res"  => "nsc> "
    case _      => "% "
  }

  /** Evaluate an action body and update the test state.
   *  @param failFn optionally map a result to a test state.
   */
  def nextTestAction[T](body: => T)(failFn: PartialFunction[T, TestState]): T = {
    val result = body
    setLastState( if (failFn isDefinedAt result) failFn(result) else genPass() )
    result
  }
  def nextTestActionExpectTrue(reason: String, body: => Boolean): Boolean = (
    nextTestAction(body) { case false => genFail(reason) }
  )
  def nextTestActionFailing(reason: String): Boolean = nextTestActionExpectTrue(reason, false)

  private def assembleTestCommand(outDir: File, logFile: File): List[String] = {
    // check whether there is a ".javaopts" file
    val argsFile  = testFile changeExtension "javaopts"
    val argString = file2String(argsFile)
    if (argString != "")
      NestUI.verbose("Found javaopts file '%s', using options: '%s'".format(argsFile, argString))

    val testFullPath = testFile.getAbsolutePath

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
      "-Dpartest.cwd="+outDir.getParent,
      "-Dpartest.test-path="+testFullPath,
      "-Dpartest.testname="+fileBase,
      "-Djavacmd="+javaCmd,
      "-Djavaccmd="+javacCmd,
      "-Duser.language=en",
      "-Duser.country=US"
    ) ++ extras

    val classpath = if (extraClasspath != "") join(extraClasspath, CLASSPATH) else CLASSPATH

    javaCmd +: (
      (JAVA_OPTS.split(' ') ++ extraJavaOptions.split(' ') ++ argString.split(' ')).map(_.trim).filter(_ != "").toList ++ Seq(
        "-classpath",
        join(outDir.toString, classpath)
      ) ++ propertyOptions ++ Seq(
        "scala.tools.nsc.MainGenericRunner",
        "-usejavacp",
        "Test",
        "jvm"
      )
    )
  }

  /** Runs command redirecting standard out and
   *  error out to output file.
   */
  private def runCommand(args: Seq[String], outFile: File): Boolean = {
    (Process(args) #> outFile !) == 0
  }

  private def execTest(outDir: File, logFile: File): Boolean = {
    val cmd = assembleTestCommand(outDir, logFile)

    pushTranscript(cmd.mkString(" \\\n  ") + " > " + logFile.getName)
    nextTestActionExpectTrue("non-zero exit code", runCommand(cmd, logFile)) || {
      _transcript append logFile.fileContents
      false
    }
  }

  override def toString = s"""Test($testIdent, lastState = $lastState)"""

  // result is unused
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

  /** Filter the diff for conditional blocks.
   *  The check file can contain lines of the form:
   *  `#partest java7`
   *  where the line contains a conventional flag name.
   *  In the diff output, these lines have the form:
   *  `> #partest java7`
   *  Blocks which don't apply are filtered out,
   *  and what remains is the desired diff.
   *  Line edit commands such as `0a1,6` don't count
   *  as diff, so return a nonempty diff only if
   *  material diff output was seen.
   *  Filtering the diff output (instead of every check
   *  file) means that we only post-process a test that
   *  might be failing, in the normal case.
   */
  def diffilter(d: String) = {
    import scala.util.Properties.javaVersion
    val prefix = "#partest"
    val margin = "> "
    val leader = margin + prefix
    // use lines in block so labeled? Default to sure, go ahead.
    def retainOn(f: String) = f match {
      case "java7"  => javaVersion startsWith "1.7"
      case "java6"  => javaVersion startsWith "1.6"
      case _        => true
    }
    if (d contains prefix) {
      val sb = new StringBuilder
      var retain = true           // use the current line
      var material = false        // saw a line of diff
      for (line <- d.lines)
        if (line startsWith leader) {
          val rest = (line stripPrefix leader).trim
          retain = retainOn(rest)
        } else if (retain) {
          if (line startsWith margin) material = true
          sb ++= line
          sb ++= EOL
        }
      if (material) sb.toString else ""
    } else d
  }

  def currentDiff = (
    if (checkFile.canRead) diffilter(compareFiles(logFile, checkFile))
    else compareContents(augmentString(file2String(logFile)).lines.toList, Nil)
  )

  val gitRunner = List("/usr/local/bin/git", "/usr/bin/git") map (f => new java.io.File(f)) find (_.canRead)
  val gitDiffOptions = "--ignore-space-at-eol --no-index " + propOrEmpty("partest.git_diff_options")
    // --color=always --word-diff

  def gitDiff(f1: File, f2: File): Option[String] = {
    try gitRunner map { git =>
      val cmd  = s"$git diff $gitDiffOptions $f1 $f2"
      val diff = Process(cmd).lines_!.drop(4).map(_ + "\n").mkString

      "\n" + diff
    }
    catch { case t: Exception => None }
  }

  /** This does something about absolute paths and file separator
   *  chars before diffing.
   */
  def normalizeLog() {
    // squashing // in paths also munges line comments, so save this innovation for another time.
    // (There are line comments in the "stub implementations" error output.)
    //val slashes   = """[/\\]+""".r
    //def squashSlashes(s: String) = slashes replaceAllIn (s, "/")
    def squashSlashes(s: String) = s replace ('\\', '/')
    val base      = squashSlashes(parentFile.getAbsolutePath + File.separator)
    val quoted    = """\Q%s\E""" format base
    val baseless  = (if (isWin) "(?i)" + quoted else quoted).r
    def canonicalize(s: String)  = baseless replaceAllIn (squashSlashes(s), "")
    logFile mapInPlace canonicalize
  }

  def diffIsOk: Boolean = {
    val diff = currentDiff
    // if diff is not empty, is update needed?
    val updating: Option[Boolean] = (
      if (diff == "") None
      else Some(fileManager.updateCheck)
    )
    pushTranscript(s"diff $logFile $checkFile")
    nextTestAction(updating) {
      case Some(true)  =>
        NestUI.verbose("Updating checkfile " + checkFile)
        checkFile writeAll file2String(logFile)
        genUpdated()
      case Some(false) =>
        // Get a word-highlighted diff from git if we can find it
        val bestDiff = if (updating.isEmpty) "" else {
          if (checkFile.canRead)
            gitDiff(logFile, checkFile) getOrElse {
              s"diff $logFile $checkFile\n$diff"
            }
          else diff
        }
        _transcript append bestDiff
        genFail("output differs")
        // TestState.fail("output differs", "output differs",
        // genFail("output differs")
        // TestState.Fail("output differs", bestDiff)
      case None        => genPass()  // redundant default case
    } getOrElse true
  }

  /** 1. Creates log file and output directory.
   *  2. Runs script function, providing log file and output directory as arguments.
   *     2b. or, just run the script without context and return a new context
   */
  def runInContext(body: => Boolean): (Boolean, LogContext) = {
    val (swr, wr) = newTestWriters()
    val succeeded = body
    (succeeded, LogContext(logFile, swr, wr))
  }

  /** Grouped files in group order, and lex order within each group. */
  def groupedFiles(files: List[File]): List[List[File]] = (
    if (files.tail.nonEmpty) {
      val grouped = files filter (_.isJavaOrScala) groupBy (_.group)
      grouped.keys.toList.sorted map (k => grouped(k) sortBy (_.getName))
    }
    else List(files)
  )

  /** Source files for the given test file. */
  def sources(file: File): List[File] = if (file.isDirectory) file.listFiles.toList else List(file)

  def newCompiler = new DirectCompiler(fileManager)

  def attemptCompile(sources: List[File]): TestState = {
    val state = newCompiler.compile(this, flagsForCompilation(sources), sources)
    if (!state.isOk)
      _transcript append ("\n" + file2String(logFile))

    state
  }

  // snort or scarf all the contributing flags files
  def flagsForCompilation(sources: List[File]): List[String] = {
    def argsplitter(s: String) = words(s) filter (_.nonEmpty)
    val perTest  = argsplitter(flagsFile.fileContents)
    val perGroup = if (testFile.isDirectory) {
      sources flatMap { f => SFile(Path(f) changeExtension "flags").safeSlurp map argsplitter getOrElse Nil }
    } else Nil
    perTest ++ perGroup
  }

  abstract class CompileRound {
    def fs: List[File]
    def result: TestState
    def description: String

    def fsString = fs map (_.toString stripPrefix parentFile.toString + "/") mkString " "
    def isOk = result.isOk
    def mkScalacString(): String = {
      val flags = file2String(flagsFile) match {
        case ""   => ""
        case s    => " " + s
      }
      s"""scalac $fsString"""
    }
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

  def compilationRounds(file: File): List[CompileRound] = (
    (groupedFiles(sources(file)) map mixedCompileGroup).flatten
  )
  def mixedCompileGroup(allFiles: List[File]): List[CompileRound] = {
    val (scalaFiles, javaFiles) = allFiles partition (_.isScala)
    val isMixed                 = javaFiles.nonEmpty && scalaFiles.nonEmpty
    val round1                  = if (scalaFiles.isEmpty) None else Some(ScalaAndJava(allFiles))
    val round2                  = if (javaFiles.isEmpty) None else Some(OnlyJava(javaFiles))
    val round3                  = if (!isMixed) None else Some(OnlyScala(scalaFiles))

    List(round1, round2, round3).flatten
  }

  def runNegTest() = runInContext {
    val rounds = compilationRounds(testFile)

    // failing means Does Not Compile
    val failing = rounds find (x => nextTestActionExpectTrue("compilation failed", x.isOk) == false)

    // which means passing if it checks and didn't crash the compiler
    def checked(r: CompileRound) = r.result match {
      case f: Crash => false
      case f        => normalizeLog(); diffIsOk
    }

    failing map (checked) getOrElse nextTestActionFailing("expected compilation failure")
  }

  def runTestCommon(andAlso: => Boolean): (Boolean, LogContext) = runInContext {
    compilationRounds(testFile).forall(x => nextTestActionExpectTrue("compilation failed", x.isOk)) && andAlso
  }

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

    runCommand(cmd, output)
  }

  def runAntTest(): (Boolean, LogContext) = {
    val (swr, wr) = newTestWriters()

    val succeeded = try {
      val binary = "-Dbinary="+(
        if      (fileManager.LATEST_LIB endsWith "build/quick/classes/library") "quick"
        else if (fileManager.LATEST_LIB endsWith "build/pack/lib/scala-library.jar") "pack"
        else if (fileManager.LATEST_LIB endsWith "dists/latest/lib/scala-library.jar/") "latest"
        else "installed"
      )
      val args = Array(binary, "-logfile", logFile.getPath, "-file", testFile.getPath)
      NestUI.verbose("ant "+args.mkString(" "))

      pushTranscript(s"ant ${args.mkString(" ")}")
      nextTestActionExpectTrue("ant failed", ant(args, logFile)) && diffIsOk
    }
    catch { // *catch-all*
      case e: Exception =>
        NestUI.warning("caught "+e)
        false
    }

    (succeeded, LogContext(logFile, swr, wr))
  }

  def extraClasspath = kind match {
    case "specialized"  => PathSettings.srcSpecLib.toString
    case _              => ""
  }
  def extraJavaOptions = kind match {
    case "instrumented" => "-javaagent:"+PathSettings.instrumentationAgentLib
    case _              => ""
  }

  def runScalacheckTest() = runTestCommon {
    NestUI verbose f"compilation of $testFile succeeded%n"

    // this classloader is test specific: its parent contains library classes and others
    val loader = {
      import PathSettings.scalaCheck
      val locations = List(outDir, scalaCheck.jfile) map (_.getAbsoluteFile.toURI.toURL)
      ScalaClassLoader.fromURLs(locations, getClass.getClassLoader)
    }
    val logWriter = new PrintStream(new FileOutputStream(logFile), true)

    def toolArgs(tool: String): List[String] = {
      def argsplitter(s: String) = words(s) filter (_.nonEmpty)
      def argsFor(f: File): List[String] = {
        import scala.util.matching.Regex
        val p    = new Regex(s"(?:.*\\s)?${tool}:(.*)?", "args")
        val max  = 10
        val src  = Path(f).toFile.chars(codec)
        val args = try {
          src.getLines take max collectFirst {
            case s if (p findFirstIn s).nonEmpty => for (m <- p findFirstMatchIn s) yield m group "args"
          }
        } finally src.close()
        args.flatten map argsplitter getOrElse Nil
      }
      sources(testFile) flatMap argsFor
    }
    def runInFramework(): Boolean = {
      import org.scalatools.testing._
      val f: Framework = loader.instantiate[Framework]("org.scalacheck.ScalaCheckFramework")
      val logger = new Logger {
        def ansiCodesSupported  = false //params.env.isSet("colors")
        def error(msg: String)  = logWriter println msg
        def warn(msg: String)   = logWriter println msg
        def info(msg: String)   = logWriter println msg
        def debug(msg: String)  = logWriter println msg
        def trace(t: Throwable) = t printStackTrace logWriter
      }
      var bad = 0
      val handler = new EventHandler {
        // testName, description, result, error
        // Result =  Success, Failure, Error, Skipped
        def handle(event: Event): Unit = event.result match {
          case Result.Success =>
          //case Result.Skipped =>   // an exhausted test is skipped, therefore bad
          case _ => bad += 1
        }
      }
      val loggers     = Array(logger)
      val r           = f.testRunner(loader, loggers).asInstanceOf[Runner2]  // why?
      val claas       = "Test"
      val fingerprint = f.tests collectFirst { case x: SubclassFingerprint if x.isModule => x }
      val args        = toolArgs("scalacheck")
      vlog(s"Run $testFile with args $args")
      // set the context class loader for scaladoc/scalacheck tests (FIX ME)
      ScalaClassLoader(testRunParams.scalaCheckParentClassLoader).asContext {
        r.run(claas, fingerprint.get, handler, args.toArray)    // synchronous?
      }
      val ok = (bad == 0)
      if (!ok) _transcript append logFile.fileContents
      ok
    }
    try nextTestActionExpectTrue("ScalaCheck test failed", runInFramework()) finally logWriter.close()
  }

  def runResidentTest() = {
    // simulate resident compiler loop
    val prompt = "\nnsc> "
    val (swr, wr) = newTestWriters()

    NestUI.verbose(this+" running test "+fileBase)
    val dir = parentFile
    val resFile = new File(dir, fileBase + ".res")

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

    def resCompile(line: String): Boolean = {
      // NestUI.verbose("compiling "+line)
      val cmdArgs = (line split ' ').toList map (fs => new File(dir, fs).getAbsolutePath)
      // NestUI.verbose("cmdArgs: "+cmdArgs)
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
    def loop(): Boolean = {
      logWriter.print(prompt)
      resReader.readLine() match {
        case null | ""  => logWriter.close() ; true
        case line       => resCompile(line) && loop()
      }
    }
    // res/t687.res depends on ignoring its compilation failure
    // and just looking at the diff, so I made them all do that
    // because this is long enough.
    if (!Output.withRedirected(logWriter)(try loop() finally resReader.close()))
      setLastState(genPass())

    normalizeLog     // put errors in a normal form
    (diffIsOk, LogContext(logFile, swr, wr))
  }

  def run(): TestState = {
    if (kind == "neg" || (kind endsWith "-neg")) runNegTest()
    else kind match {
      case "pos"          => runTestCommon(true)
      case "ant"          => runAntTest()
      case "scalacheck"   => runScalacheckTest()
      case "res"          => runResidentTest()
      case "scalap"       => runScalapTest()
      case "script"       => runScriptTest()
      case _              => runTestCommon(execTest(outDir, logFile) && diffIsOk)
    }

    lastState
  }

  def runScalapTest() = runTestCommon {
    val isPackageObject = testFile.getName startsWith "package"
    val className       = testFile.getName.stripSuffix(".scala").capitalize + (if (!isPackageObject) "" else ".package")
    val loader          = ScalaClassLoader.fromURLs(List(outDir.toURI.toURL), this.getClass.getClassLoader)
    val byteCode        = ByteCode forClass (loader loadClass className)
    val result          = decompileScala(byteCode.bytes, isPackageObject)

    logFile writeAll result
    diffIsOk
  }
  def runScriptTest() = {
    import scala.sys.process._
    val (swr, wr) = newTestWriters()

    val args = file2String(testFile changeExtension "args")
    val cmdFile = if (isWin) testFile changeExtension "bat" else testFile
    val succeeded = (((cmdFile + " " + args) #> logFile !) == 0) && diffIsOk

    (succeeded, LogContext(logFile, swr, wr))
  }

  def cleanup() {
    if (lastState.isOk)
      logFile.delete()
    if (!isPartestDebug)
      Directory(outDir).deleteRecursively()
  }
}

case class TestRunParams(val scalaCheckParentClassLoader: ScalaClassLoader)

/** Extended by Ant- and ConsoleRunner for running a set of tests. */
trait DirectRunner {
  def fileManager: FileManager

  import PartestDefaults.{ numThreads, waitTime }

  Thread.setDefaultUncaughtExceptionHandler(
    new Thread.UncaughtExceptionHandler {
      def uncaughtException(thread: Thread, t: Throwable) {
        val t1 = Exceptional unwrap t
        System.err.println(s"Uncaught exception on thread $thread: $t1")
        t1.printStackTrace()
      }
    }
  )
  def runTestsForFiles(kindFiles: List[File], kind: String): List[TestState] = {

    NestUI.resetTestNumber(kindFiles.size)

    // this special class loader is for the benefit of scaladoc tests, which need a class path
    import PathSettings.{ testInterface, scalaCheck }
    val allUrls           = scalaCheck.toURL :: testInterface.toURL :: fileManager.latestUrls
    val parentClassLoader = ScalaClassLoader fromURLs allUrls
    // add scalacheck.jar to a special classloader, but use our loader as parent with test-interface
    //val parentClassLoader = ScalaClassLoader fromURLs (List(scalaCheck.toURL), getClass().getClassLoader)
    val pool              = Executors newFixedThreadPool numThreads
    val manager           = new RunnerManager(kind, fileManager, TestRunParams(parentClassLoader))
    val futures           = kindFiles map (f => pool submit callable(manager runTest f))

    pool.shutdown()
    Try (pool.awaitTermination(waitTime) {
      throw TimeoutException(waitTime)
    }) match {
      case Success(_) => futures map (_.get)
      case Failure(e) =>
        e match {
          case TimeoutException(d)      =>
            NestUI warning "Thread pool timeout elapsed before all tests were complete!"
          case ie: InterruptedException =>
            NestUI warning "Thread pool was interrupted"
            ie.printStackTrace()
        }
        pool.shutdownNow()     // little point in continuing
        // try to get as many completions as possible, in case someone cares
        val results = for (f <- futures) yield {
          try {
            Some(f.get(0, NANOSECONDS))
          } catch {
            case _: Throwable => None
          }
        }
        results.flatten
    }
  }
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

/** Use a Runner to run a test. */
class RunnerManager(kind: String, fileManager: FileManager, params: TestRunParams) {
  import fileManager._
  fileManager.CLASSPATH += File.pathSeparator + PathSettings.scalaCheck
  fileManager.CLASSPATH += File.pathSeparator + PathSettings.diffUtils // needed to put diffutils on test/partest's classpath

  def runTest(testFile: File): TestState = {
    val runner = new Runner(testFile, fileManager, params)

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    if (fileManager.failed && !runner.logFile.canRead)
      runner.genPass()
    else {
      val (state, elapsed) =
        try timed(runner.run())
        catch {
          case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
        }
      NestUI.reportTest(state)
      runner.cleanup()
      state
    }
  }
}
