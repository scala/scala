// Copyright 2005-2017 LAMP/EPFL and Lightbend, Inc.

package scala.tools.nsc.interpreter.shell

import java.io.{BufferedReader, PrintWriter}
import java.nio.file.Files
import java.util.concurrent.TimeUnit

import scala.PartialFunction.{cond => when}
import scala.Predef.{println => _, _}
import scala.annotation.tailrec
import scala.language.{existentials, implicitConversions}
import scala.util.Properties.jdkHome
import scala.reflect.classTag
import scala.reflect.internal.util.ScalaClassLoader._
import scala.reflect.internal.util.{BatchSourceFile, NoPosition, ScalaClassLoader}
import scala.reflect.io.{AbstractFile, Directory, File, Path}
import scala.tools.asm.ClassReader
import scala.tools.util.PathResolver
import scala.tools.nsc.Settings
import scala.tools.nsc.util.{stackTraceString, stringFromStream}
import scala.tools.nsc.interpreter.{AbstractOrMissingHandler, Repl, IMain, Phased, jline}
import scala.tools.nsc.interpreter.Results.{Error, Incomplete, Success}
import scala.tools.nsc.interpreter.StdReplTags._
import scala.tools.nsc.util.Exceptional.rootCause
import scala.util.control.ControlThrowable
import scala.collection.JavaConverters._



/** The Scala interactive shell. This part provides the user interface,
  * with evaluation and auto-complete handled by IMain.
  *
  * There should be no direct dependency of this code on the compiler;
  * it should all go through the `intp` reference to the interpreter,
  * or maybe eventually even over the wire to a remote compiler.
  *
  * @author Moez A. Abdel-Gawad
  * @author Lex Spoon
  */
class ILoop(config: ShellConfig, inOverride: BufferedReader = null,
            protected val out: PrintWriter = new PrintWriter(Console.out, true)) extends LoopCommands {
  import config._

  // If set before calling run(), the provided interpreter will be used
  // (until a destructive reset command is issued -- TODO: delegate resetting to the repl)
  // Set by createInterpreter, closeInterpreter (and CompletionTest)
  var intp: Repl = _

  def Repl(config: ShellConfig, interpreterSettings: Settings, out: PrintWriter) =
    new IMain(interpreterSettings, None, interpreterSettings, new ReplReporterImpl(config, interpreterSettings, out))


  // Set by run and interpretAllFrom (to read input from file).
  private var in: InteractiveReader = _

  // TODO: the new interface should make settings a ctor arg of ILoop,
  // so that this can be a lazy val
  private lazy val defaultIn: InteractiveReader =
    if (batchMode) SimpleReader(batchText)
    else if (inOverride != null) SimpleReader(inOverride, out, interactive = true)
    else if (haveInteractiveConsole) new jline.JlineReader(isAcross = isAcross, isPaged = isPaged)
    else SimpleReader()


  private val interpreterInitialized = new java.util.concurrent.CountDownLatch(1)


  // TODO: move echo and friends to ReplReporterImpl
  // When you know you are most likely breaking into the middle
  // of a line being typed.  This softens the blow.
  protected def echoAndRefresh(msg: String) = {
    echo("\n" + msg)
    in.redrawLine()
  }
  protected var mum = false
  protected def echo(msg: String) = if (!mum || isReplDebug) {
    out println msg
    out.flush()
  }
  // turn off our echo
  def echoOff[A](op: => A): A = {
    val saved = mum
    mum = true
    try op finally mum = saved
  }

  private def printShellInterrupt(): Unit = {
    out print ShellConfig.InterruptedString
  }

  protected def asyncMessage(msg: String): Unit = {
    if (isReplInfo || isReplPower)
      echoAndRefresh(msg)
  }

  override def echoCommandMessage(msg: String): Unit = {
    intp.reporter.withoutTruncating { intp.reporter.printMessage(msg) }
  }


  import scala.tools.nsc.interpreter.ReplStrings.{words, string2codeQuoted}

  def welcome = enversion(welcomeString)

  /** Print a welcome message! */
  def printWelcome(): Unit = {
    replinfo(s"[info] started at ${new java.util.Date}")
    if (!welcome.isEmpty) echo(welcome)
  }



  def history = in.history

  /** A reverse list of commands to replay if the user requests a :replay */
  var replayCommandStack: List[String] = Nil

  /** A list of commands to replay if the user requests a :replay */
  def replayCommands = replayCommandStack.reverse

  /** Record a command for replay should the user request a :replay */
  def addReplay(cmd: String) = replayCommandStack ::= cmd

  def savingReplayStack[T](body: => T): T = {
    val saved = replayCommandStack
    try body
    finally replayCommandStack = saved
  }


  /** Close the interpreter and set the var to null.
    *
    * Used by sbt.
    */
  def closeInterpreter() {
    if (intp ne null) {
      intp.close()
      intp = null
    }
  }


  /** Create a new interpreter.
    *
    * Used by sbt.
    */
  def createInterpreter(interpreterSettings: Settings): Unit = {
    intp = Repl(config, interpreterSettings, out)
  }

  /** Show the history */
  lazy val historyCommand = new LoopCommand("history", "show the history (optional num is commands to show)") {
    override def usage = "[num]"
    def defaultLines = 20

    def apply(line: String): Result = {
      if (history eq NoHistory)
        return "No history available."

      val xs      = words(line)
      val current = history.index
      val count   = try xs.head.toInt catch { case _: Exception => defaultLines }
      val lines   = history.asStrings takeRight count
      val offset  = current - lines.size + 1

      for ((line, index) <- lines.zipWithIndex)
        echo("%3d  %s".format(index + offset, line))
    }
  }


  /** Search the history */
  def searchHistory(_cmdline: String) {
    val cmdline = _cmdline.toLowerCase
    val offset  = history.index - history.size + 1

    for ((line, index) <- history.asStrings.zipWithIndex ; if line.toLowerCase contains cmdline)
      echo("%d %s".format(index + offset, line))
  }

  import LoopCommand.{cmd, nullary}

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("edit", "<id>|<line>", "edit history", editCommand),
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    historyCommand,
    cmd("h?", "<string>", "search the history", searchHistory),
    cmd("imports", "[name name ...]", "show import history, identifying sources of names", importsCommand),
    cmd("implicits", "[-v]", "show the implicits in scope", implicitsCommand),
    cmd("javap", "<path|class>", "disassemble a file or class name", javapCommand),
    cmd("line", "<id>|<line>", "place line(s) at the end of history", lineCommand),
    cmd("load", "<path>", "interpret lines in a file", loadCommand, fileCompletion),
    cmd("paste", "[-raw] [path]", "enter paste mode or paste a file", pasteCommand, fileCompletion),
    nullary("power", "enable power user mode", powerCmd),
    nullary("quit", "exit the interpreter", () => Result(keepRunning = false, None)),
    cmd("replay", "[options]", "reset the repl and replay all previous commands", replayCommand, settingsCompletion),
    cmd("require", "<path>", "add a jar to the classpath", require),
    cmd("reset", "[options]", "reset the repl to its initial state, forgetting all session entries", resetCommand, settingsCompletion),
    cmd("save", "<path>", "save replayable session to a file", saveCommand, fileCompletion),
    shCommand,
    cmd("settings", "<options>", "update compiler options, if possible; see reset", changeSettings, settingsCompletion),
    nullary("silent", "disable/enable automatic printing of results", verbosity),
    cmd("type", "[-v] <expr>", "display the type of an expression without evaluating it", typeCommand),
    cmd("kind", "[-v] <expr>", "display the kind of expression's type", kindCommand),
    nullary("warnings", "show the suppressed warnings from the most recent line which had any", warningsCommand)
  )

  /** Power user commands */
  lazy val powerCommands: List[LoopCommand] = List(
    cmd("phase", "<phase>", "set the implicit phase for power commands", phaseCommand)
  )

  // complete filename
  val fileCompletion: Completion = new Completion {
    def resetVerbosity(): Unit = ()
    val emptyWord    = """(\s+)$""".r.unanchored
    val directorily  = """(\S*/)$""".r.unanchored
    val trailingWord = """(\S+)$""".r.unanchored
    def listed(i: Int, dir: Option[Path]) =
      dir.filter(_.isDirectory).map(d => CompletionResult(i, d.toDirectory.list.map(_.name).toList)).getOrElse(NoCompletions)
    def listedIn(dir: Directory, name: String) = dir.list.filter(_.name.startsWith(name)).map(_.name).toList
    def complete(buffer: String, cursor: Int): CompletionResult =
      buffer.substring(0, cursor) match {
        case emptyWord(s)        => listed(cursor, Directory.Current)
        case directorily(s)      => listed(cursor, Option(Path(s)))
        case trailingWord(s) =>
          val f = File(s)
          val (i, maybes) =
            if (f.isFile) (cursor - s.length, List(f.toAbsolute.path))
            else if (f.isDirectory) (cursor - s.length, List(s"${f.toAbsolute.path}/"))
            else if (f.parent.exists) (cursor - f.name.length, listedIn(f.parent.toDirectory, f.name))
            else (-1, Nil)
          if (maybes.isEmpty) NoCompletions else CompletionResult(i, maybes)
        case _                   => NoCompletions
      }
  }

  // complete settings name
  val settingsCompletion: Completion = new Completion {
    def resetVerbosity(): Unit = ()
    val trailingWord = """(\S+)$""".r.unanchored
    def complete(buffer: String, cursor: Int): CompletionResult = {
      buffer.substring(0, cursor) match {
        case trailingWord(s) =>
          val maybes = intp.visibleSettings.filter(_.name.startsWith(s)).map(_.name)
                               .filterNot(when(_) { case "-"|"-X"|"-Y" => true }).toList.sorted
          if (maybes.isEmpty) NoCompletions else CompletionResult(cursor - s.length, maybes)
        case _ => NoCompletions
      }
    }
  }


  private def importsCommand(line: String): Result =
    intp.importsCommandInternal(words(line)) mkString ("\n")


  private def findToolsJar() = PathResolver.SupplementalLocations.platformTools

  private def addToolsJarToLoader() = {
    val cl = findToolsJar() match {
      case Some(tools) => ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader)
      case _           => intp.classLoader
    }
    if (Javap.isAvailable(cl)) {
      repldbg(":javap available.")
      cl
    }
    else {
      repldbg(":javap unavailable: no tools.jar at " + jdkHome)
      intp.classLoader
    }
  }

  protected def newJavap() = JavapClass(addToolsJarToLoader(), intp.reporter.out, intp)

  private lazy val javap =
    try newJavap()
    catch {
      case t: ControlThrowable => throw t
      case t: Throwable =>
        repldbg("javap: " + rootCause(t))
        repltrace(stackTraceString(rootCause(t)))
        NoJavap
    }



  private def implicitsCommand(line: String): Result = {
    val (implicits, res) = intp.implicitsCommandInternal(line)
    implicits foreach echoCommandMessage
    res
  }

  // Still todo: modules.
  private def typeCommand(line0: String): Result = {
    line0.trim match {
      case "" => ":type [-v] <expression>"
      case s  =>
        val verbose = s startsWith "-v "
        val (sig, verboseSig) = intp.typeCommandInternal(s stripPrefix "-v " trim, verbose)
        if (verbose) echoCommandMessage("// Type signature")
        echoCommandMessage(sig)
        if (!verboseSig.isEmpty) echoCommandMessage("\n// Internal Type structure\n"+ verboseSig)
        ()
    }
  }

  private def kindCommand(expr: String): Result = {
    expr.trim match {
      case "" => ":kind [-v] <expression>"
      case s  => intp.kindCommandInternal(s stripPrefix "-v " trim, verbose = s startsWith "-v ")
    }
  }

  private def warningsCommand(): Result = {
    if (intp.lastWarnings.isEmpty)
      "Can't find any cached warnings."
    else
      intp.lastWarnings foreach { case (pos, msg) => intp.reporter.warning(pos, msg) }
  }

  private def javapCommand(line: String): Result = {
    if (javap == null)
      s":javap unavailable, no tools.jar at $jdkHome.  Set JDK_HOME."
    else if (line == "")
      Javap.helpText
    else
      javap(words(line)) foreach { res =>
        if (res.isError) return s"Failed: ${res.value}"
        else res.show()
      }
  }

  private def pathToPhaseWrapper = intp.originalPath("$r") + ".phased.atCurrent"

  private def phaseCommand(name: String): Result = {
    val phased: Phased = intp.power.phased
    import phased.NoPhaseName

    if (name == "clear") {
      phased.set(NoPhaseName)
      intp.clearExecutionWrapper()
      "Cleared active phase."
    }
    else if (name == "") phased.get match {
      case NoPhaseName => "Usage: :phase <expr> (e.g. typer, erasure.next, erasure+3)"
      case ph          => "Active phase is '%s'.  (To clear, :phase clear)".format(phased.get)
    }
    else {
      val what = phased.parse(name)
      if (what.isEmpty || !phased.set(what))
        "'" + name + "' does not appear to represent a valid phase."
      else {
        intp.setExecutionWrapper(pathToPhaseWrapper)
        val activeMessage =
          if (what.toString.length == name.length) "" + what
          else "%s (%s)".format(what, name)

        "Active phase is now: " + activeMessage
      }
    }
  }

  /** Available commands */
  def commands: List[LoopCommand] = standardCommands ++ (
    if (isReplPower) powerCommands else Nil
  )

  val replayQuestionMessage =
    """|That entry seems to have slain the compiler.  Shall I replay
       |your session? I can re-run each line except the last one.
       |[y/n]
    """.trim.stripMargin

  private val crashRecovery: PartialFunction[Throwable, Boolean] = {
    case ex: Throwable =>
      val (err, explain) = (
        if (intp.initializeComplete)
          (stackTraceString(ex), "")
        else
          (ex.getMessage, "The compiler did not initialize.\n")
      )
      echo(err)

      ex match {
        case _: NoSuchMethodError | _: NoClassDefFoundError =>
          echo("\nUnrecoverable error.")
          throw ex
        case _  =>
          def fn(): Boolean =
            try in.readYesOrNo(explain + replayQuestionMessage, { echo("\nYou must enter y or n.") ; fn() })
            catch { case _: RuntimeException => false }

          if (fn()) replay()
          else echo("\nAbandoning crashed session.")
      }
      true
  }

  // after process line, OK continue, ERR break, or EOF all done
  object LineResults extends Enumeration {
    type LineResult = Value
    val EOF, ERR, OK = Value
  }
  import LineResults.LineResult

  // return false if repl should exit
  def processLine(line: String): Boolean = {
    // Long timeout here to avoid test failures under heavy load.
    interpreterInitialized.await(10, TimeUnit.MINUTES)

    command(line) match {
      case Result(false, _)      => false
      case Result(_, Some(line)) => addReplay(line) ; true
      case _                     => true
    }
  }

  lazy val prompt = encolor(promptText)

  private def readOneLine() = {
    out.flush()
    in readLine prompt
  }

  /** The main read-eval-print loop for the repl.  It calls
   *  command() for each line of input, and stops when
   *  command() returns false.
   */
  final def loop(): LineResult = loop(readOneLine())

  @tailrec final def loop(line: String): LineResult = {
    import LineResults._
    if (line == null) EOF
    else if (try processLine(line) catch crashRecovery) loop(readOneLine())
    else ERR
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(file: File, verbose: Boolean = false): Unit = {
    // Saving `in` is not factored out because we don't want to encourage doing this everywhere (the new design shouldn't rely on mutation)
    val savedIn = in
    try
      savingReplayStack {
        // `applyReader` will `close()` `fileReader` before returning,
        // so, keep `in` pointing at `fileReader` until that's done.
        file applyReader { fileReader =>
          echo(s"Loading $file...")
          in = SimpleReader(fileReader, out, interactive = verbose, verbose = verbose)
          loop()
        }
      }
    finally in = savedIn
  }

  private def changeSettings(line: String): Result = {
    def showSettings() = for (s <- { intp.userSetSettings }.toSeq.sorted) echo(s.toString)
    if (line.isEmpty) showSettings()
    else { intp.updateSettings(words(line)) ; () }
  }

  /** create a new interpreter and replay the given commands */
  def replayCommand(line: String): Unit = {
    def run(destructive: Boolean): Unit = {
      if (destructive) createInterpreter(intp.settings) else reset()
      replay()
    }
    if (line.isEmpty) run(destructive = false)
    else if (intp.updateSettings(words(line))) run(destructive = true)
  }
  /** Announces as it replays. */
  def replay(): Unit = {
    if (replayCommandStack.isEmpty)
      echo("Nothing to replay.")
    else for (cmd <- replayCommands) {
      echo("Replaying: " + cmd)  // flush because maybe cmd will have its own output
      command(cmd)
      echo("")
    }
  }
  /** `reset` the interpreter in an attempt to start fresh.
   *  Supplying settings creates a new compiler.
   */
  def resetCommand(line: String): Unit = {
    def run(destructive: Boolean): Unit = {
      echo("Resetting interpreter state.")
      if (replayCommandStack.nonEmpty) {
        echo("Forgetting this session history:\n")
        replayCommands foreach echo
        echo("")
        replayCommandStack = Nil
      }
      if (intp.namedDefinedTerms.nonEmpty)
        echo("Forgetting all expression results and named terms: " + intp.namedDefinedTerms.mkString(", "))
      if (intp.definedTypes.nonEmpty)
        echo("Forgetting defined types: " + intp.definedTypes.mkString(", "))
      if (destructive) createInterpreter(intp.settings) else reset()
    }
    if (line.isEmpty) run(destructive = false)
    else if (intp.updateSettings(words(line))) run(destructive = true)
  }
  /** Resets without announcements. */
  def reset() {
    intp.reset()
    unleashAndSetPhase()
  }

  def lineCommand(what: String): Result = editCommand(what, None)

  // :edit id or :edit line
  def editCommand(what: String): Result = editCommand(what, ShellConfig.EDITOR)

  def editCommand(what: String, editor: Option[String]): Result = {
    def diagnose(code: String): Unit = paste.incomplete("The edited code is incomplete!\n", "<edited>", code)

    def edit(text: String): Result = editor match {
      case Some(ed) =>
        val tmp = File.makeTemp()
        tmp.writeAll(text)
        try {
          val pr = new ProcessResult(s"$ed ${tmp.path}")
          pr.exitCode match {
            case 0 =>
              tmp.safeSlurp() match {
                case Some(edited) if edited.trim.isEmpty => echo("Edited text is empty.")
                case Some(edited) =>
                  echo(edited.lines map ("+" + _) mkString "\n")
                  val res = intp interpret edited
                  if (res == Incomplete) diagnose(edited)
                  else {
                    history.historicize(edited)
                    Result(lineToRecord = Some(edited), keepRunning = true)
                  }
                case None => echo("Can't read edited text. Did you delete it?")
              }
            case x => echo(s"Error exit from $ed ($x), ignoring")
          }
        } finally {
          tmp.delete()
        }
      case None =>
        if (history.historicize(text)) echo("Placing text in recent history.")
        else echo(f"No EDITOR defined and you can't change history, echoing your text:%n$text")
    }

    // if what is a number, use it as a line number or range in history
    def isNum = what forall (c => c.isDigit || c == '-' || c == '+')
    // except that "-" means last value
    def isLast = (what == "-")
    if (isLast || !isNum) {
      intp.requestDefining(if (isLast) intp.mostRecentVar else what) match {
        case Some(req) => edit(req.line)
        case None      => echo(s"No symbol in scope: $what")
      }
    } else try {
      val s = what
      // line 123, 120+3, -3, 120-123, 120-, note -3 is not 0-3 but (cur-3,cur)
      val (start, len) =
        if ((s indexOf '+') > 0) {
          val (a,b) = s splitAt (s indexOf '+')
          (a.toInt, b.drop(1).toInt)
        } else {
          (s indexOf '-') match {
            case -1 => (s.toInt, 1)
            case 0  => val n = s.drop(1).toInt ; (history.index - n, n)
            case _ if s.last == '-' => val n = s.init.toInt ; (n, history.index - n)
            case i  => val n = s.take(i).toInt ; (n, s.drop(i+1).toInt - n)
          }
        }
      val index = (start - 1) max 0
      val text = history.asStrings(index, index + len) mkString "\n"
      edit(text)
    } catch {
      case _: NumberFormatException => echo(s"Bad range '$what'")
        echo("Use line 123, 120+3, -3, 120-123, 120-, note -3 is not 0-3 but (cur-3,cur)")
    }
  }

  /** fork a shell and run a command */
  lazy val shCommand = new LoopCommand("sh", "run a shell command (result is implicitly => List[String])") {
    override def usage = "<command line>"
    def apply(line: String): Result = line match {
      case ""   => showUsage()
      case _    =>
        val toRun = s"new ${classOf[ProcessResult].getName}(${string2codeQuoted(line)})"
        intp interpret toRun
        ()
    }
  }

  def withFile[A](filename: String)(action: File => A): Option[A] = intp.withLabel(filename) {
    val res = Some(File(filename)) filter (_.exists) map action
    if (res.isEmpty) intp.reporter.warning(NoPosition, s"File `$filename' does not exist.")  // courtesy side-effect
    res
  }

  def loadCommand(arg: String): Result = {
    def run(file: String, verbose: Boolean) = withFile(file) { f =>
      interpretAllFrom(f, verbose)
      Result recording s":load $arg"
    } getOrElse Result.default

    words(arg) match {
      case "-v" :: file :: Nil => run(file, verbose = true)
      case file :: Nil         => run(file, verbose = false)
      case _                   => echo("usage: :load -v file") ; Result.default
    }
  }

  def saveCommand(filename: String): Result = (
    if (filename.isEmpty) echo("File name is required.")
    else if (replayCommandStack.isEmpty) echo("No replay commands in session")
    else File(filename).printlnAll(replayCommands: _*)
  )

  /** Adds jar file to the current classpath. Jar will only be added if it
   *  does not contain classes that already exist on the current classpath.
   *
   *  Importantly, `require` adds jars to the classpath ''without'' resetting
   *  the state of the interpreter. This is in contrast to `replay` which can
   *  be used to add jars to the classpath and which creates a new instance of
   *  the interpreter and replays all interpreter expressions.
   */
  def require(arg: String): Unit = {
    val f = File(arg).normalize

    val jarFile = AbstractFile.getDirectory(new java.io.File(arg))
    if (jarFile == null) {
      echo(s"Cannot load '$arg'")
      return
    }

    def flatten(f: AbstractFile): Iterator[AbstractFile] =
      if (f.isClassContainer) f.iterator.flatMap(flatten)
      else Iterator(f)

    val entries = flatten(jarFile)

    def classNameOf(classFile: AbstractFile): String = {
      val input = classFile.input
      try {
        val reader = new ClassReader(input)
        reader.getClassName.replace('/', '.')
      } finally {
        input.close()
      }
    }
    def alreadyDefined(clsName: String) = intp.classLoader.tryToLoadClass(clsName).isDefined
    val existingClass = entries.filter(_.hasExtension("class")).map(classNameOf).find(alreadyDefined)

    if (!f.exists) echo(s"The path '$f' doesn't seem to exist.")
    else if (existingClass.nonEmpty) echo(s"The path '$f' cannot be loaded, it contains a classfile that already exists on the classpath: ${existingClass.get}")
    else {
      intp.addUrlsToClassPath(f.toURI.toURL)
      echo("Added '%s' to classpath.".format(f.path))
      repldbg("Added '%s'. Your new classpath is:\n\"%s\"".format(f.path, intp.classPathString))
    }
  }

  def powerCmd(): Result = {
    if (isReplPower) "Already in power mode."
    else enablePowerMode(isDuringInit = false)
  }
  def enablePowerMode(isDuringInit: Boolean) = {
    config.power setValue true
    unleashAndSetPhase()
    asyncEcho(isDuringInit, powerBannerMessage)
  }

  private def powerBannerMessage: String =
    powerBanner.option map {
      case f if f.getName == "classic" => intp.power.classic
      case f => Files.readAllLines(f.toPath).asScala.mkString("\n")
    } getOrElse intp.power.banner

  private def unleashAndSetPhase() =
    if (isReplPower) {
      intp.power.unleash()

      intp.reporter.suppressOutput {
        (powerInitCode.option
          map (f => Files.readAllLines(f.toPath).asScala.toList)
          getOrElse intp.power.initImports
          foreach intp.interpret)

        phaseCommand("typer") // Set the phase to "typer"
      }
    }

  def asyncEcho(async: Boolean, msg: => String) {
    if (async) asyncMessage(msg)
    else echo(msg)
  }

  def verbosity() = {
    intp.reporter.togglePrintResults()
    replinfo(s"Result printing is ${ if (intp.reporter.printResults) "on" else "off" }.")
  }

  /** Run one command submitted by the user.  Two values are returned:
   *  (1) whether to keep running, (2) the line to record for replay, if any.
   */
  def command(line: String): Result = {
    if (line startsWith ":") colonCommand(line)
    else {
      if (!intp.initializeCompiler()) Result(keepRunning = false, None)  // Notice failure to create compiler
      else Result(keepRunning = true, interpretStartingWith(line))
    }
  }

  private def readWhile(cond: String => Boolean) = {
    Iterator continually in.readLine("") takeWhile (x => x != null && cond(x))
  }

  /* :paste -raw file
   * or
   * :paste < EOF
   *   your code
   * EOF
   * :paste <~ EOF
   *   ~your code
   * EOF
   */
  def pasteCommand(arg: String): Result = {
    var shouldReplay: Option[String] = None
    var label = "<pastie>"
    def result = Result(keepRunning = true, shouldReplay)
    val (raw, file, margin) =
      if (arg.isEmpty) (false, None, None)
      else {
        def maybeRaw(ss: List[String]) = if (ss.nonEmpty && ss.head == "-raw") (true, ss.tail) else (false, ss)
        def maybeHere(ss: List[String]) =
          if (ss.nonEmpty && ss.head.startsWith("<")) (ss.head.dropWhile(_ == '<'), ss.tail)
          else (null, ss)

        val (raw0, ss0) = maybeRaw(words(arg))
        val (margin0, ss1) = maybeHere(ss0)
        val file0 = ss1 match {
          case Nil      => null
          case x :: Nil => x
          case _        => echo("usage: :paste [-raw] file | < EOF") ; return result
        }
        (raw0, Option(file0), Option(margin0))
      }
    val code = (file, margin) match {
      case (Some(name), None) =>
        label = name
        withFile(name) { f =>
          shouldReplay = Some(s":paste $arg")
          val s = f.slurp.trim
          if (s.isEmpty) echo(s"File contains no code: $f")
          else echo(s"Pasting file $f...")
          s
        } getOrElse ""
      case (eof, _) =>
        echo(s"// Entering paste mode (${ eof getOrElse "ctrl-D" } to finish)\n")
        val delimiter = eof orElse config.pasteDelimiter.option
        val input = readWhile(s => delimiter.isEmpty || delimiter.get != s) mkString "\n"
        val text = (
          margin filter (_.nonEmpty) map {
            case "-" => input.lines map (_.trim) mkString "\n"
            case m   => input stripMargin m.head   // ignore excess chars in "<<||"
          } getOrElse input
        ).trim
        if (text.isEmpty) echo("\n// Nothing pasted, nothing gained.\n")
        else echo("\n// Exiting paste mode, now interpreting.\n")
        text
    }
    def interpretCode() = {
      if (intp.withLabel(label)(intp interpret code) == Incomplete)
        paste.incomplete("The pasted code is incomplete!\n", label, code)
    }
    def compileCode() = paste.compilePaste(label = label, code = code)

    if (code.nonEmpty) {
      if (raw || paste.isPackaged(code)) compileCode() else interpretCode()
    }
    result
  }

  private val continueText   = {
    val text   = enversion(continueString)
    val margin = promptText.lines.toList.last.length - text.length
    if (margin > 0) " " * margin + text else text
  }

  private object paste extends Pasted(config.promptText, encolor(continueText), continueText) {
    def interpret(line: String) = intp interpret line
    def echo(message: String)   = ILoop.this echo message

    val leadingElement = raw"(?s)\s*(package\s|/)".r
    def isPackaged(code: String): Boolean = {
      leadingElement.findPrefixMatchOf(code)
        .map(m => if (m.group(1) == "/") intp.isPackaged(code) else true)
        .getOrElse(false)
    }

    // if input is incomplete, wrap and compile for diagnostics.
    def incomplete(message: String, label: String, code: String): Boolean = {
      echo(message)
      val errless = intp.compileSources(new BatchSourceFile(label, s"object pastel {\n$code\n}"))
      if (errless) echo("No error found in incomplete source.")
      errless
    }

    def compilePaste(label: String, code: String): Boolean = {
      val errless = intp.compileSources(new BatchSourceFile(label, code))
      if (!errless) echo("There were compilation errors!")
      errless
    }
  }

  private object invocation {
    // used during loop
    def unapply(line: String): Boolean =
      intp.mostRecentVar != "" && Parsed.looksLikeInvocation(line)
  }

  private val lineComment = """\s*//.*""".r   // all comment

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  final def interpretStartingWith(code: String): Option[String] = {
    // signal completion non-completion input has been received
    in.completion.resetVerbosity()

    /* Here we place ourselves between the user and the interpreter and examine
     * the input they are ostensibly submitting.  We intervene in several cases:
     *
     * 1) If the line starts with "scala> " it is assumed to be an interpreter paste.
     * 2) If the line starts with "." (but not ".." or "./") it is treated as an invocation
     *    on the previous result.
     * 3) If the Completion object's execute returns Some(_), we inject that value
     *    and avoid the interpreter, as it's likely not valid scala code.
     */
    code match {
      case "" | lineComment() => None // empty or line comment, do nothing
      case paste() =>
        paste.transcript(Iterator(code) ++ readWhile(!paste.isPromptOnly(_))) match {
          case Some(s) => interpretStartingWith(s)
          case _       => None
        }
      case invocation() => interpretStartingWith(intp.mostRecentVar + code)
      case _ =>
        intp.interpret(code) match {
          case Error      => None
          case Success    => Some(code)
          case Incomplete =>
            if (in.interactive && code.endsWith("\n\n")) {
              echo("You typed two blank lines.  Starting a new command.")
              None
            } else {
              val prefix = code + "\n"
              in.completion.withPartialInput(prefix) {
                in.readLine(paste.ContinuePrompt) match {
                  case null =>
                    // we know compilation is going to fail since we're at EOF and the
                    // parser thinks the input is still incomplete, but since this is
                    // a file being read non-interactively we want to fail.
                    // TODO: is this true ^^^^^^^^^^^^^^^^?
                    // So we send it straight to the compiler for the nice error message.
                    intp.compileString(code)
                    None
                  case line =>
                    interpretStartingWith(prefix + line) // not in tailpos!
                }
              }
            }
        }
    }
  }

  /** Actions to cram in parallel while collecting first user input at prompt.
    * Run with output muted both from ILoop and from the intp reporter.
    */
  private def interpretPreamble = {
    // Bind intp somewhere out of the regular namespace where
    // we can get at it in generated code.
    intp.quietBind(intp.namedParam[Repl]("$intp", intp)(tagOfRepl, classTag[Repl]))

    // Auto-run code via some setting.
    (config.replAutorunCode.option
      flatMap (f => File(f).safeSlurp())
      foreach (intp quietRun _)
      )
    // power mode setup
    if (isReplPower)
      enablePowerMode(isDuringInit = true)

    for (f <- filesToLoad) {
      loadCommand(f)
      addReplay(s":load $f")
    }
    for (f <- filesToPaste) {
      pasteCommand(f)
      addReplay(s":paste $f")
    }
  }

  /** Start an interpreter with the given settings.
   *  @return true if successful
   */
  def run(interpreterSettings: Settings): Boolean = savingContextLoader {
    if (!batchMode) printWelcome()

    in = defaultIn

    // let them start typing, using the splash reader (which avoids tab completion)
    val firstLine =
      SplashLoop.readLine(in, prompt) {
        if (intp eq null) createInterpreter(interpreterSettings)
        intp.reporter.withoutPrintingResults {
          intp.initializeCompiler()
          interpreterInitialized.countDown() // TODO: move to reporter.compilerInitialized ?

          if (intp.reporter.hasErrors) {
            echo("Interpreter encountered errors during initialization!")
            throw new InterruptedException
          }

          echoOff { interpretPreamble }

          // scala/bug#7418 Now that the interpreter is initialized, and `interpretPreamble` has populated the symbol table,
          // enable TAB completion (we do this before blocking on input from the splash loop,
          // so that it can offer tab completion as soon as we're ready).
          if (doCompletion)
            in.initCompletion(new ReplCompletion(intp) {
              override def shellCompletion(buffer: String, cursor: Int): Option[CompletionResult] =
                if (buffer.startsWith(":")) Some(colonCompletion(buffer, cursor).complete(buffer, cursor))
                else None
            })

        }
      } orNull // null is used by readLine to signal EOF (`loop` will exit)

    // start full loop (if initialization was successful)
    try
      loop(firstLine) match {
        case LineResults.EOF if in.interactive => printShellInterrupt(); true
        case LineResults.ERR => false
        case _ => true
      }
    catch AbstractOrMissingHandler()
    finally closeInterpreter()
  }
}

object ILoop {
  implicit def loopToInterpreter(repl: ILoop): Repl = repl.intp

  // Designed primarily for use by test code: take a String with a
  // bunch of code, and prints out a transcript of what it would look
  // like if you'd just typed it into the repl.
  def runForTranscript(code: String, settings: Settings, inSession: Boolean = false): String = {
    import java.io.{BufferedReader, OutputStreamWriter, StringReader}
    import java.lang.System.{lineSeparator => EOL}

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val output = new PrintWriter(new OutputStreamWriter(ostream), true) {
          // skip margin prefix for continuation lines, unless preserving session text for test
          // should test for repl.paste.ContinueString or config.continueText.contains(ch)
          override def write(str: String) =
            if (inSession || (str.exists(ch => ch != ' ' && ch != '|'))) super.write(str)
        }
        val input = new BufferedReader(new StringReader(s"${code.trim}${EOL}")) {
          override def readLine(): String = {
            mark(1)    // default buffer is 8k
            val c = read()
            if (c == -1 || c == 4) {
              null
            } else {
              reset()
              val s = super.readLine()
              // helping out by printing the line being interpreted.
              output.println(s)
              s
            }
          }
        }

        val config = ShellConfig(settings)
        val repl = new ILoop(config, input, output) {
          // remove welcome message as it has versioning info (for reproducible test results),
          override def welcome = ""
        }
        if (settings.classpath.isDefault)
          settings.classpath.value = sys.props("java.class.path")

        repl.run(settings)
      }
    }
  }

  /** Creates an interpreter loop with default settings and feeds
   *  the given code to it as input.
   */
  def run(code: String, sets: Settings = new Settings): String = {
    import java.io.{BufferedReader, OutputStreamWriter, StringReader}

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val input    = new BufferedReader(new StringReader(code))
        val output   = new PrintWriter(new OutputStreamWriter(ostream), true)
        val config   = ShellConfig(sets)
        val repl     = new ILoop(config, input, output) {
          // remove welcome message as it has versioning info (for reproducible test results),
          override def welcome = ""
        }

        if (sets.classpath.isDefault)
          sets.classpath.value = sys.props("java.class.path")

        repl.run(sets)
      }
    }
  }
  def run(lines: List[String]): String = run(lines map (_ + "\n") mkString)
}
