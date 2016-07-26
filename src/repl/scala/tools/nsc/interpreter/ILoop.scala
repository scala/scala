/* NSC -- new Scala compiler
 * Copyright 2005-2016 LAMP/EPFL
 * @author Alexander Spoon
 */
package scala
package tools.nsc
package interpreter

import scala.language.{ implicitConversions, existentials }
import scala.annotation.tailrec
import Predef.{ println => _, _ }
import interpreter.session._
import StdReplTags._
import scala.tools.asm.ClassReader
import scala.util.Properties.jdkHome
import scala.tools.nsc.util.{ ClassPath, stringFromStream }
import scala.reflect.classTag
import scala.reflect.internal.util.{ BatchSourceFile, ScalaClassLoader, NoPosition }
import ScalaClassLoader._
import scala.reflect.io.File
import scala.tools.util._
import io.AbstractFile
import scala.concurrent.{ ExecutionContext, Await, Future }
import ExecutionContext.Implicits._
import java.io.BufferedReader

import scala.util.{ Try, Success, Failure }

/** The Scala interactive shell.  It provides a read-eval-print loop
 *  around the Interpreter class.
 *  After instantiation, clients should call the main() method.
 *
 *  If no in0 is specified, then input will come from the console, and
 *  the class will attempt to provide input editing feature such as
 *  input history.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author  Lex Spoon
 *  @version 1.2
 */
class ILoop(in0: Option[BufferedReader], protected val out: JPrintWriter)
                extends AnyRef
                   with LoopCommands
{
  def this(in0: BufferedReader, out: JPrintWriter) = this(Some(in0), out)
  def this() = this(None, new JPrintWriter(Console.out, true))

  @deprecated("use `intp` instead.", "2.9.0") def interpreter = intp
  @deprecated("use `intp` instead.", "2.9.0") def interpreter_= (i: Interpreter): Unit = intp = i

  var in: InteractiveReader = _   // the input stream from which commands come
  var settings: Settings = _
  var intp: IMain = _

  private var globalFuture: Future[Boolean] = _

  /** Print a welcome message! */
  def printWelcome(): Unit = {
    Option(replProps.welcome) filter (!_.isEmpty) foreach echo
    replinfo("[info] started at " + new java.util.Date)
  }

  protected def asyncMessage(msg: String) {
    if (isReplInfo || isReplPower)
      echoAndRefresh(msg)
  }

  override def echoCommandMessage(msg: String) {
    intp.reporter printUntruncatedMessage msg
  }

  lazy val power = new Power(intp, new StdReplVals(this))(tagOfStdReplVals, classTag[StdReplVals])
  def history = in.history

  // classpath entries added via :cp
  @deprecated("use reset, replay or require to update class path", since = "2.11.0")
  var addedClasspath: String = ""

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
  def savingReader[T](body: => T): T = {
    val saved = in
    try body
    finally in = saved
  }

  /** Close the interpreter and set the var to null. */
  def closeInterpreter() {
    if (intp ne null) {
      intp.close()
      intp = null
    }
  }

  class ILoopInterpreter extends IMain(settings, out) {
    override protected def parentClassLoader =
      settings.explicitParentLoader.getOrElse( classOf[ILoop].getClassLoader )
  }

  /** Create a new interpreter. */
  def createInterpreter() {
    if (addedClasspath != "")
      settings.classpath append addedClasspath

    intp = new ILoopInterpreter
  }

  /** print a friendly help message */
  def helpCommand(line: String): Result = line match {
    case ""                => helpSummary()
    case CommandMatch(cmd) => echo(f"%n${cmd.help}")
    case _                 => ambiguousError(line)
  }
  private def helpSummary() = {
    val usageWidth = commands map (_.usageMsg.length) max
    val formatStr  = s"%-${usageWidth}s %s"

    echo("All commands can be abbreviated, e.g., :he instead of :help.")

    for (cmd <- commands) echo(formatStr.format(cmd.usageMsg, cmd.help))
  }
  private def ambiguousError(cmd: String): Result = {
    matchingCommands(cmd) match {
      case Nil  => echo(cmd + ": no such command.  Type :help for help.")
      case xs   => echo(cmd + " is ambiguous: did you mean " + xs.map(":" + _.name).mkString(" or ") + "?")
    }
    Result(keepRunning = true, None)
  }
  // this lets us add commands willy-nilly and only requires enough command to disambiguate
  private def matchingCommands(cmd: String) = commands filter (_.name startsWith cmd)
  private object CommandMatch {
    def unapply(name: String): Option[LoopCommand] =
      matchingCommands(name) match {
        case x :: Nil => Some(x)
        case xs       => xs find (_.name == name)  // accept an exact match
      }
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

  // When you know you are most likely breaking into the middle
  // of a line being typed.  This softens the blow.
  protected def echoAndRefresh(msg: String) = {
    echo("\n" + msg)
    in.redrawLine()
  }
  protected var mum = false
  protected def echo(msg: String) = if (!mum) {
    out println msg
    out.flush()
  }
  // turn off intp reporter and our echo
  def mumly[A](op: => A): A =
    if (isReplDebug) op
    else intp beQuietDuring {
      val saved = mum
      mum = true
      try op finally mum = saved
    }

  /** Search the history */
  def searchHistory(_cmdline: String) {
    val cmdline = _cmdline.toLowerCase
    val offset  = history.index - history.size + 1

    for ((line, index) <- history.asStrings.zipWithIndex ; if line.toLowerCase contains cmdline)
      echo("%d %s".format(index + offset, line))
  }

  /** Prompt to print when awaiting input */
  def prompt = replProps.prompt

  import LoopCommand.{ cmd, nullary }

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("edit", "<id>|<line>", "edit history", editCommand),
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    historyCommand,
    cmd("h?", "<string>", "search the history", searchHistory),
    cmd("imports", "[name name ...]", "show import history, identifying sources of names", importsCommand),
    cmd("implicits", "[-v]", "show the implicits in scope", intp.implicitsCommand),
    cmd("javap", "<path|class>", "disassemble a file or class name", javapCommand),
    cmd("line", "<id>|<line>", "place line(s) at the end of history", lineCommand),
    cmd("load", "<path>", "interpret lines in a file", loadCommand),
    cmd("paste", "[-raw] [path]", "enter paste mode or paste a file", pasteCommand),
    nullary("power", "enable power user mode", powerCmd),
    nullary("quit", "exit the interpreter", () => Result(keepRunning = false, None)),
    cmd("replay", "[options]", "reset the repl and replay all previous commands", replayCommand),
    cmd("require", "<path>", "add a jar to the classpath", require),
    cmd("reset", "[options]", "reset the repl to its initial state, forgetting all session entries", resetCommand),
    cmd("save", "<path>", "save replayable session to a file", saveCommand),
    shCommand,
    cmd("settings", "<options>", "update compiler options, if possible; see reset", changeSettings),
    nullary("silent", "disable/enable automatic printing of results", verbosity),
    cmd("type", "[-v] <expr>", "display the type of an expression without evaluating it", typeCommand),
    cmd("kind", "[-v] <expr>", "display the kind of expression's type", kindCommand),
    nullary("warnings", "show the suppressed warnings from the most recent line which had any", warningsCommand)
  )

  /** Power user commands */
  lazy val powerCommands: List[LoopCommand] = List(
    cmd("phase", "<phase>", "set the implicit phase for power commands", phaseCommand)
  )

  private def importsCommand(line: String): Result = {
    val tokens    = words(line)
    val handlers  = intp.languageWildcardHandlers ++ intp.importHandlers

    handlers.filterNot(_.importedSymbols.isEmpty).zipWithIndex foreach {
      case (handler, idx) =>
        val (types, terms) = handler.importedSymbols partition (_.name.isTypeName)
        val imps           = handler.implicitSymbols
        val found          = tokens filter (handler importsSymbolNamed _)
        val typeMsg        = if (types.isEmpty) "" else types.size + " types"
        val termMsg        = if (terms.isEmpty) "" else terms.size + " terms"
        val implicitMsg    = if (imps.isEmpty) "" else imps.size + " are implicit"
        val foundMsg       = if (found.isEmpty) "" else found.mkString(" // imports: ", ", ", "")
        val statsMsg       = List(typeMsg, termMsg, implicitMsg) filterNot (_ == "") mkString ("(", ", ", ")")

        intp.reporter.printMessage("%2d) %-30s %s%s".format(
          idx + 1,
          handler.importString,
          statsMsg,
          foundMsg
        ))
    }
  }

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

  protected def newJavap() = JavapClass(addToolsJarToLoader(), new IMain.ReplStrippingWriter(intp), intp)

  private lazy val javap = substituteAndLog[Javap]("javap", NoJavap)(newJavap())

  // Still todo: modules.
  private def typeCommand(line0: String): Result = {
    line0.trim match {
      case "" => ":type [-v] <expression>"
      case s  => intp.typeCommandInternal(s stripPrefix "-v " trim, verbose = s startsWith "-v ")
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

  private def changeSettings(line: String): Result = {
    def showSettings() = for (s <- settings.userSetSettings.toSeq.sorted) echo(s.toString)
    if (line.isEmpty) showSettings() else { updateSettings(line) ; () }
  }
  private def updateSettings(line: String) = {
    val (ok, rest) = settings.processArguments(words(line), processAll = false)
    ok && rest.isEmpty
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
    val phased: Phased = power.phased
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
        if (intp.isInitializeComplete)
          (intp.global.throwableAsString(ex), "")
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
    import scala.concurrent.duration._
    Await.ready(globalFuture, 10.minutes) // Long timeout here to avoid test failures under heavy load.

    command(line) match {
      case Result(false, _)      => false
      case Result(_, Some(line)) => addReplay(line) ; true
      case _                     => true
    }
  }

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
  def interpretAllFrom(file: File, verbose: Boolean = false) {
    savingReader {
      savingReplayStack {
        file applyReader { reader =>
          in = if (verbose) new SimpleReader(reader, out, interactive = true) with EchoReader
               else SimpleReader(reader, out, interactive = false)
          echo(s"Loading $file...")
          loop()
        }
      }
    }
  }

  /** create a new interpreter and replay the given commands */
  def replayCommand(line: String): Unit = {
    def run(destructive: Boolean): Unit = {
      if (destructive) createInterpreter() else reset()
      replay()
    }
    if (line.isEmpty) run(destructive = false)
    else if (updateSettings(line)) run(destructive = true)
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
      if (destructive) createInterpreter() else reset()
    }
    if (line.isEmpty) run(destructive = false)
    else if (updateSettings(line)) run(destructive = true)
  }
  /** Resets without announcements. */
  def reset() {
    intp.reset()
    unleashAndSetPhase()
  }

  def lineCommand(what: String): Result = editCommand(what, None)

  // :edit id or :edit line
  def editCommand(what: String): Result = editCommand(what, Properties.envOrNone("EDITOR"))

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
                  if (res == IR.Incomplete) diagnose(edited)
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
      val name = if (isLast) intp.mostRecentVar else what
      val sym = intp.symbolOfIdent(name)
      intp.prevRequestList collectFirst { case r if r.defines contains sym => r } match {
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

  @deprecated("use reset, replay or require to update class path", since = "2.11.0")
  def addClasspath(arg: String): Unit = {
    val f = File(arg).normalize
    if (f.exists) {
      addedClasspath = ClassPath.join(addedClasspath, f.path)
      intp.addUrlsToClassPath(f.toURI.toURL)
      echo("Added '%s' to classpath.".format(f.path, intp.global.classPath.asClassPathString))
      repldbg("Added '%s'.  Your new classpath is:\n\"%s\"".format(f.path, intp.global.classPath.asClassPathString))
    }
    else echo("The path '" + f + "' doesn't seem to exist.")
  }

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
      addedClasspath = ClassPath.join(addedClasspath, f.path)
      intp.addUrlsToClassPath(f.toURI.toURL)
      echo("Added '%s' to classpath.".format(f.path, intp.global.classPath.asClassPathString))
      repldbg("Added '%s'.  Your new classpath is:\n\"%s\"".format(f.path, intp.global.classPath.asClassPathString))
    }
  }

  def powerCmd(): Result = {
    if (isReplPower) "Already in power mode."
    else enablePowerMode(isDuringInit = false)
  }
  def enablePowerMode(isDuringInit: Boolean) = {
    replProps.power setValue true
    unleashAndSetPhase()
    asyncEcho(isDuringInit, power.banner)
  }
  private def unleashAndSetPhase() = if (isReplPower) {
    power.unleash()
    intp beSilentDuring phaseCommand("typer") // Set the phase to "typer"
  }

  def asyncEcho(async: Boolean, msg: => String) {
    if (async) asyncMessage(msg)
    else echo(msg)
  }

  def verbosity() = {
    intp.printResults = !intp.printResults
    replinfo(s"Result printing is ${ if (intp.printResults) "on" else "off" }.")
  }

  /** Run one command submitted by the user.  Two values are returned:
   *  (1) whether to keep running, (2) the line to record for replay, if any.
   */
  def command(line: String): Result = {
    if (line startsWith ":") colonCommand(line.tail)
    else if (intp.global == null) Result(keepRunning = false, None)  // Notice failure to create compiler
    else Result(keepRunning = true, interpretStartingWith(line))
  }

  private val commandish = """(\S+)(?:\s+)?(.*)""".r

  private def colonCommand(line: String): Result = line.trim match {
    case ""                                  => helpSummary()
    case commandish(CommandMatch(cmd), rest) => cmd(rest)
    case commandish(name, _)                 => ambiguousError(name)
    case _                                   => echo("?")
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
        val delimiter = eof orElse replProps.pasteDelimiter.option
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
      if (intp.withLabel(label)(intp interpret code) == IR.Incomplete)
        paste.incomplete("The pasted code is incomplete!\n", label, code)
    }
    def compileCode() = paste.compilePaste(label = label, code = code)

    if (code.nonEmpty) {
      if (raw || paste.isPackaged(code)) compileCode() else interpretCode()
    }
    result
  }

  private object paste extends Pasted(prompt) {
    def interpret(line: String) = intp interpret line
    def echo(message: String)   = ILoop.this echo message

    val leadingElement = raw"(?s)\s*(package\s|/)".r
    def isPackaged(code: String): Boolean = {
      leadingElement.findPrefixMatchOf(code)
        .map(m => if (m.group(1) == "/") intp.parse.packaged(code) else true)
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
    def unapply(line: String): Boolean = Completion.looksLikeInvocation(line)
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
      case ""                                       => None
      case lineComment()                            => None                 // line comment, do nothing
      case paste() if !paste.running                => paste.transcript(Iterator(code) ++ readWhile(!paste.isPromptOnly(_))) match {
                                                         case Some(s) => interpretStartingWith(s)
                                                         case _       => None
                                                       }
      case invocation() if intp.mostRecentVar != "" => interpretStartingWith(intp.mostRecentVar + code)
      case _                                        => intp.interpret(code) match {
        case IR.Error      => None
        case IR.Success    => Some(code)
        case IR.Incomplete if in.interactive && code.endsWith("\n\n") =>
          echo("You typed two blank lines.  Starting a new command.")
          None
        case IR.Incomplete =>
          val saved = intp.partialInput
          intp.partialInput = code + "\n"
          try {
            in.readLine(paste.ContinuePrompt) match {
              case null =>
                // we know compilation is going to fail since we're at EOF and the
                // parser thinks the input is still incomplete, but since this is
                // a file being read non-interactively we want to fail.  So we send
                // it straight to the compiler for the nice error message.
                intp.compileString(code)
                None
              case line => interpretStartingWith(s"$code\n$line")
            }
          } finally intp.partialInput = saved
      }
    }
  }

  /** Tries to create a jline.InteractiveReader, falling back to SimpleReader,
   *  unless settings or properties are such that it should start with SimpleReader.
   *  The constructor of the InteractiveReader must take a Completion strategy,
   *  supplied as a `() => Completion`; the Completion object provides a concrete Completer.
   */
  def chooseReader(settings: Settings): InteractiveReader = {
    if (settings.Xnojline || Properties.isEmacsShell) SimpleReader()
    else {
      type Completer = () => Completion
      type ReaderMaker = Completer => InteractiveReader

      def instantiater(className: String): ReaderMaker = completer => {
        if (settings.debug) Console.println(s"Trying to instantiate an InteractiveReader from $className")
        Class.forName(className).getConstructor(classOf[Completer]).
          newInstance(completer).
          asInstanceOf[InteractiveReader]
      }

      def mkReader(maker: ReaderMaker) = maker { () =>
        if (settings.noCompletion) NoCompletion else new PresentationCompilerCompleter(intp)
      }

      def internalClass(kind: String) = s"scala.tools.nsc.interpreter.$kind.InteractiveReader"
      val readerClasses = sys.props.get("scala.repl.reader").toStream ++ Stream(internalClass("jline"), internalClass("jline_embedded"))
      val readers = readerClasses map (cls => Try { mkReader(instantiater(cls)) })

      val reader = (readers collect { case Success(reader) => reader } headOption) getOrElse SimpleReader()

      if (settings.debug) {
        val readerDiags = (readerClasses, readers).zipped map {
          case (cls, Failure(e)) => s"  - $cls --> \n\t" + scala.tools.nsc.util.stackTraceString(e) + "\n"
          case (cls, Success(_)) => s"  - $cls OK"
        }
        Console.println(s"All InteractiveReaders tried: ${readerDiags.mkString("\n","\n","\n")}")
      }
      reader
    }
  }

  /** Start an interpreter with the given settings.
   *  @return true if successful
   */
  def process(settings: Settings): Boolean = savingContextLoader {
    def newReader = in0.fold(chooseReader(settings))(r => SimpleReader(r, out, interactive = true))

    /** Reader to use before interpreter is online. */
    def preLoop = {
      val sr = SplashReader(newReader) { r =>
        in = r
        in.postInit()
      }
      in = sr
      SplashLoop(sr, prompt)
    }

    /* Actions to cram in parallel while collecting first user input at prompt.
     * Run with output muted both from ILoop and from the intp reporter.
     */
    def loopPostInit(): Unit = mumly {
      // Bind intp somewhere out of the regular namespace where
      // we can get at it in generated code.
      intp.quietBind(NamedParam[IMain]("$intp", intp)(tagOfIMain, classTag[IMain]))

      // Auto-run code via some setting.
      ( replProps.replAutorunCode.option
          flatMap (f => File(f).safeSlurp())
          foreach (intp quietRun _)
      )
      // power mode setup
      if (isReplPower) {
        replProps.power setValue true
        unleashAndSetPhase()
        asyncMessage(power.banner)
      }
      loadInitFiles()
      // SI-7418 Now, and only now, can we enable TAB completion.
      in.postInit()
    }
    def loadInitFiles(): Unit = settings match {
      case settings: GenericRunnerSettings =>
        for (f <- settings.loadfiles.value) {
          loadCommand(f)
          addReplay(s":load $f")
        }
        for (f <- settings.pastefiles.value) {
          pasteCommand(f)
          addReplay(s":paste $f")
        }
      case _ =>
    }
    // wait until after startup to enable noisy settings
    def withSuppressedSettings[A](body: => A): A = {
      val ss = this.settings
      import ss._
      val noisy = List(Xprint, Ytyperdebug)
      val noisesome = noisy.exists(!_.isDefault)
      val current = (Xprint.value, Ytyperdebug.value)
      if (isReplDebug || !noisesome) body
      else {
        this.settings.Xprint.value = List.empty
        this.settings.Ytyperdebug.value = false
        try body
        finally {
          Xprint.value       = current._1
          Ytyperdebug.value  = current._2
          intp.global.printTypings = current._2
        }
      }
    }
    def startup(): String = withSuppressedSettings {
      // starting
      printWelcome()

      // let them start typing
      val splash = preLoop
      splash.start()

      // while we go fire up the REPL
      try {
        // don't allow ancient sbt to hijack the reader
        savingReader {
          createInterpreter()
        }
        intp.initializeSynchronous()
        globalFuture = Future successful true
        if (intp.reporter.hasErrors) {
          echo("Interpreter encountered errors during initialization!")
          null
        } else {
          loopPostInit()
          val line = splash.line           // what they typed in while they were waiting
          if (line == null) {              // they ^D
            try out print Properties.shellInterruptedString
            finally closeInterpreter()
          }
          line
        }
      } finally splash.stop()
    }
    this.settings = settings
    startup() match {
      case null    => false
      case line    =>
        try loop(line) match {
          case LineResults.EOF => out print Properties.shellInterruptedString
          case _               =>
        }
        catch AbstractOrMissingHandler()
        finally closeInterpreter()
        true
    }
  }

  @deprecated("use `process` instead", "2.9.0")
  def main(settings: Settings): Unit = process(settings) //used by sbt
}

object ILoop {
  implicit def loopToInterpreter(repl: ILoop): IMain = repl.intp

  // Designed primarily for use by test code: take a String with a
  // bunch of code, and prints out a transcript of what it would look
  // like if you'd just typed it into the repl.
  def runForTranscript(code: String, settings: Settings, inSession: Boolean = false): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val output = new JPrintWriter(new OutputStreamWriter(ostream), true) {
          // skip margin prefix for continuation lines, unless preserving session text for test
          // should test for repl.paste.ContinueString or replProps.continueText.contains(ch)
          override def write(str: String) =
            if (!inSession && (str forall (ch => ch.isWhitespace || ch == '|'))) ()
            else super.write(str)
        }
        val input = new BufferedReader(new StringReader(code.trim + "\n")) {
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
        val repl = new ILoop(input, output)
        if (settings.classpath.isDefault)
          settings.classpath.value = sys.props("java.class.path")

        repl process settings
      }
    }
  }

  /** Creates an interpreter loop with default settings and feeds
   *  the given code to it as input.
   */
  def run(code: String, sets: Settings = new Settings): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val input    = new BufferedReader(new StringReader(code))
        val output   = new JPrintWriter(new OutputStreamWriter(ostream), true)
        val repl     = new ILoop(input, output)

        if (sets.classpath.isDefault)
          sets.classpath.value = sys.props("java.class.path")

        repl process sets
      }
    }
  }
  def run(lines: List[String]): String = run(lines map (_ + "\n") mkString)
}
