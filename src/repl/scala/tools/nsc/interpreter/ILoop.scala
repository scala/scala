/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import java.io.{ BufferedReader, FileReader }
import session._
import scala.annotation.tailrec
import scala.util.Properties.{ jdkHome, javaVersion, versionString, javaVmName }
import util.{ ClassPath, Exceptional, stringFromWriter, stringFromStream }
import io.{ File, Directory }
import util.ScalaClassLoader
import ScalaClassLoader._
import scala.tools.util._
import scala.language.{implicitConversions, existentials}
import scala.reflect.classTag
import StdReplTags._
import scala.concurrent.{ ExecutionContext, Await, Future, future }
import ExecutionContext.Implicits._

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

  @deprecated("Use `intp` instead.", "2.9.0") def interpreter = intp
  @deprecated("Use `intp` instead.", "2.9.0") def interpreter_= (i: Interpreter): Unit = intp = i

  var in: InteractiveReader = _   // the input stream from which commands come
  var settings: Settings = _
  var intp: IMain = _

  private var globalFuture: Future[Boolean] = _

  /** Print a welcome message */
  def printWelcome() {
    echo(s"""
      |Welcome to Scala $versionString ($javaVmName, Java $javaVersion).
      |Type in expressions to have them evaluated.
      |Type :help for more information.""".trim.stripMargin
    )
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
    outer =>

    override lazy val formatting = new Formatting {
      def prompt = ILoop.this.prompt
    }
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
  def helpCommand(line: String): Result = {
    if (line == "") helpSummary()
    else uniqueCommand(line) match {
      case Some(lc) => echo("\n" + lc.help)
      case _        => ambiguousError(line)
    }
  }
  private def helpSummary() = {
    val usageWidth  = commands map (_.usageMsg.length) max
    val formatStr   = "%-" + usageWidth + "s %s"

    echo("All commands can be abbreviated, e.g. :he instead of :help.")

    commands foreach { cmd =>
      echo(formatStr.format(cmd.usageMsg, cmd.help))
    }
  }
  private def ambiguousError(cmd: String): Result = {
    matchingCommands(cmd) match {
      case Nil  => echo(cmd + ": no such command.  Type :help for help.")
      case xs   => echo(cmd + " is ambiguous: did you mean " + xs.map(":" + _.name).mkString(" or ") + "?")
    }
    Result(keepRunning = true, None)
  }
  private def matchingCommands(cmd: String) = commands filter (_.name startsWith cmd)
  private def uniqueCommand(cmd: String): Option[LoopCommand] = {
    // this lets us add commands willy-nilly and only requires enough command to disambiguate
    matchingCommands(cmd) match {
      case List(x)  => Some(x)
      // exact match OK even if otherwise appears ambiguous
      case xs       => xs find (_.name == cmd)
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
  protected def echo(msg: String) = {
    out println msg
    out.flush()
  }

  /** Search the history */
  def searchHistory(_cmdline: String) {
    val cmdline = _cmdline.toLowerCase
    val offset  = history.index - history.size + 1

    for ((line, index) <- history.asStrings.zipWithIndex ; if line.toLowerCase contains cmdline)
      echo("%d %s".format(index + offset, line))
  }

  private val currentPrompt = Properties.shellPromptString

  /** Prompt to print when awaiting input */
  def prompt = currentPrompt

  import LoopCommand.{ cmd, nullary }

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("cp", "<path>", "add a jar or directory to the classpath", addClasspath),
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    historyCommand,
    cmd("h?", "<string>", "search the history", searchHistory),
    cmd("imports", "[name name ...]", "show import history, identifying sources of names", importsCommand),
    cmd("implicits", "[-v]", "show the implicits in scope", intp.implicitsCommand),
    cmd("javap", "<path|class>", "disassemble a file or class name", javapCommand),
    cmd("load", "<path>", "load and interpret a Scala file", loadCommand),
    nullary("paste", "enter paste mode: all input up to ctrl-D compiled together", pasteCommand),
    nullary("power", "enable power user mode", powerCmd),
    nullary("quit", "exit the interpreter", () => Result(keepRunning = false, None)),
    nullary("replay", "reset execution and replay all previous commands", replay),
    nullary("reset", "reset the repl to its initial state, forgetting all session entries", resetCommand),
    shCommand,
    nullary("silent", "disable/enable automatic printing of results", verbosity),
    cmd("type", "[-v] <expr>", "display the type of an expression without evaluating it", typeCommand),
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

  private def findToolsJar() = {
    val jdkPath = Directory(jdkHome)
    val jar     = jdkPath / "lib" / "tools.jar" toFile

    if (jar isFile)
      Some(jar)
    else if (jdkPath.isDirectory)
      jdkPath.deepFiles find (_.name == "tools.jar")
    else None
  }
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

  protected def newJavap() =
    JavapClass(addToolsJarToLoader(), new IMain.ReplStrippingWriter(intp), Some(intp))

  private lazy val javap = substituteAndLog[Javap]("javap", NoJavap)(newJavap())

  // Still todo: modules.
  private def typeCommand(line0: String): Result = {
    line0.trim match {
      case ""                      => ":type [-v] <expression>"
      case s if s startsWith "-v " => intp.typeCommandInternal(s stripPrefix "-v " trim, verbose = true)
      case s                       => intp.typeCommandInternal(s, verbose = false)
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
      ":javap unavailable, no tools.jar at %s.  Set JDK_HOME.".format(jdkHome)
    else if (line == "")
      ":javap [-lcsvp] [path1 path2 ...]"
    else
      javap(words(line)) foreach { res =>
        if (res.isError) return "Failed: " + res.value
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
      echo(intp.global.throwableAsString(ex))

      ex match {
        case _: NoSuchMethodError | _: NoClassDefFoundError =>
          echo("\nUnrecoverable error.")
          throw ex
        case _  =>
          def fn(): Boolean =
            try in.readYesOrNo(replayQuestionMessage, { echo("\nYou must enter y or n.") ; fn() })
            catch { case _: RuntimeException => false }

          if (fn()) replay()
          else echo("\nAbandoning crashed session.")
      }
      true
  }

  // return false if repl should exit
  def processLine(line: String): Boolean = {
    import scala.concurrent.duration._
    Await.ready(globalFuture, 60.seconds)

    (line ne null) && (command(line) match {
      case Result(false, _)      => false
      case Result(_, Some(line)) => addReplay(line) ; true
      case _                     => true
    })
  }

  private def readOneLine() = {
    out.flush()
    in readLine prompt
  }

  /** The main read-eval-print loop for the repl.  It calls
   *  command() for each line of input, and stops when
   *  command() returns false.
   */
  @tailrec final def loop() {
    if ( try processLine(readOneLine()) catch crashRecovery )
      loop()
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(file: File) {
    savingReader {
      savingReplayStack {
        file applyReader { reader =>
          in = SimpleReader(reader, out, interactive = false)
          echo("Loading " + file + "...")
          loop()
        }
      }
    }
  }

  /** create a new interpreter and replay the given commands */
  def replay() {
    reset()
    if (replayCommandStack.isEmpty)
      echo("Nothing to replay.")
    else for (cmd <- replayCommands) {
      echo("Replaying: " + cmd)  // flush because maybe cmd will have its own output
      command(cmd)
      echo("")
    }
  }
  def resetCommand() {
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

    reset()
  }
  def reset() {
    intp.reset()
    unleashAndSetPhase()
  }

  /** fork a shell and run a command */
  lazy val shCommand = new LoopCommand("sh", "run a shell command (result is implicitly => List[String])") {
    override def usage = "<command line>"
    def apply(line: String): Result = line match {
      case ""   => showUsage()
      case _    =>
        val toRun = classOf[ProcessResult].getName + "(" + string2codeQuoted(line) + ")"
        intp interpret toRun
        ()
    }
  }

  def withFile(filename: String)(action: File => Unit) {
    val f = File(filename)

    if (f.exists) action(f)
    else echo("That file does not exist")
  }

  def loadCommand(arg: String) = {
    var shouldReplay: Option[String] = None
    withFile(arg)(f => {
      interpretAllFrom(f)
      shouldReplay = Some(":load " + arg)
    })
    Result(keepRunning = true, shouldReplay)
  }

  def addClasspath(arg: String): Unit = {
    val f = File(arg).normalize
    if (f.exists) {
      addedClasspath = ClassPath.join(addedClasspath, f.path)
      val totalClasspath = ClassPath.join(settings.classpath.value, addedClasspath)
      echo("Added '%s'.  Your new classpath is:\n\"%s\"".format(f.path, totalClasspath))
      replay()
    }
    else echo("The path '" + f + "' doesn't seem to exist.")
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
  private def unleashAndSetPhase() {
    if (isReplPower) {
      power.unleash()
      // Set the phase to "typer"
      intp beSilentDuring phaseCommand("typer")
    }
  }

  def asyncEcho(async: Boolean, msg: => String) {
    if (async) asyncMessage(msg)
    else echo(msg)
  }

  def verbosity() = {
    val old = intp.printResults
    intp.printResults = !old
    echo("Switched " + (if (old) "off" else "on") + " result printing.")
  }

  /** Run one command submitted by the user.  Two values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): Result = {
    if (line startsWith ":") {
      val cmd = line.tail takeWhile (x => !x.isWhitespace)
      uniqueCommand(cmd) match {
        case Some(lc) => lc(line.tail stripPrefix cmd dropWhile (_.isWhitespace))
        case _        => ambiguousError(cmd)
      }
    }
    else if (intp.global == null) Result(keepRunning = false, None)  // Notice failure to create compiler
    else Result(keepRunning = true, interpretStartingWith(line))
  }

  private def readWhile(cond: String => Boolean) = {
    Iterator continually in.readLine("") takeWhile (x => x != null && cond(x))
  }

  def pasteCommand(): Result = {
    echo("// Entering paste mode (ctrl-D to finish)\n")
    val code = readWhile(_ => true) mkString "\n"
    echo("\n// Exiting paste mode, now interpreting.\n")
    intp interpret code
    ()
  }

  private object paste extends Pasted {
    val ContinueString = "     | "
    val PromptString   = "scala> "

    def interpret(line: String): Unit = {
      echo(line.trim)
      intp interpret line
      echo("")
    }

    def transcript(start: String) = {
      echo("\n// Detected repl transcript paste: ctrl-D to finish.\n")
      apply(Iterator(start) ++ readWhile(_.trim != PromptString.trim))
    }
  }
  import paste.{ ContinueString, PromptString }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  def interpretStartingWith(code: String): Option[String] = {
    // signal completion non-completion input has been received
    in.completion.resetVerbosity()

    def reallyInterpret = {
      val reallyResult = intp.interpret(code)
      (reallyResult, reallyResult match {
        case IR.Error       => None
        case IR.Success     => Some(code)
        case IR.Incomplete  =>
          if (in.interactive && code.endsWith("\n\n")) {
            echo("You typed two blank lines.  Starting a new command.")
            None
          }
          else in.readLine(ContinueString) match {
            case null =>
              // we know compilation is going to fail since we're at EOF and the
              // parser thinks the input is still incomplete, but since this is
              // a file being read non-interactively we want to fail.  So we send
              // it straight to the compiler for the nice error message.
              intp.compileString(code)
              None

            case line => interpretStartingWith(code + "\n" + line)
          }
      })
    }

    /** Here we place ourselves between the user and the interpreter and examine
     *  the input they are ostensibly submitting.  We intervene in several cases:
     *
     *  1) If the line starts with "scala> " it is assumed to be an interpreter paste.
     *  2) If the line starts with "." (but not ".." or "./") it is treated as an invocation
     *     on the previous result.
     *  3) If the Completion object's execute returns Some(_), we inject that value
     *     and avoid the interpreter, as it's likely not valid scala code.
     */
    if (code == "") None
    else if (!paste.running && code.trim.startsWith(PromptString)) {
      paste.transcript(code)
      None
    }
    else if (Completion.looksLikeInvocation(code) && intp.mostRecentVar != "") {
      interpretStartingWith(intp.mostRecentVar + code)
    }
    else if (code.trim startsWith "//") {
      // line comment, do nothing
      None
    }
    else
      reallyInterpret._2
  }

  // runs :load `file` on any files passed via -i
  def loadFiles(settings: Settings) = settings match {
    case settings: GenericRunnerSettings =>
      for (filename <- settings.loadfiles.value) {
        val cmd = ":load " + filename
        command(cmd)
        addReplay(cmd)
        echo("")
      }
    case _ =>
  }

  /** Tries to create a JLineReader, falling back to SimpleReader:
   *  unless settings or properties are such that it should start
   *  with SimpleReader.
   */
  def chooseReader(settings: Settings): InteractiveReader = {
    if (settings.Xnojline || Properties.isEmacsShell)
      SimpleReader()
    else try new JLineReader(
      if (settings.noCompletion) NoCompletion
      else new JLineCompletion(intp)
    )
    catch {
      case ex @ (_: Exception | _: NoClassDefFoundError) =>
        echo("Failed to created JLineReader: " + ex + "\nFalling back to SimpleReader.")
        SimpleReader()
    }
  }

  private def loopPostInit() {
    in match {
      case x: JLineReader => x.consoleReader.postInit
      case _              =>
    }
    // Bind intp somewhere out of the regular namespace where
    // we can get at it in generated code.
    intp.quietBind(NamedParam[IMain]("$intp", intp)(tagOfIMain, classTag[IMain]))
    // Auto-run code via some setting.
    ( replProps.replAutorunCode.option
        flatMap (f => io.File(f).safeSlurp())
        foreach (intp quietRun _)
    )
    // classloader and power mode setup
    intp.setContextClassLoader()
    if (isReplPower) {
      replProps.power setValue true
      unleashAndSetPhase()
      asyncMessage(power.banner)
    }
  }
  def process(settings: Settings): Boolean = savingContextLoader {
    this.settings = settings
    createInterpreter()

    // sets in to some kind of reader depending on environmental cues
    in = in0.fold(chooseReader(settings))(r => SimpleReader(r, out, interactive = true))
    globalFuture = future {
      intp.initializeSynchronous()
      loopPostInit()
      loadFiles(settings)
      !intp.reporter.hasErrors
    }
    printWelcome()

    try loop()
    catch AbstractOrMissingHandler()
    finally closeInterpreter()

    true
  }

  @deprecated("Use `process` instead", "2.9.0")
  def main(settings: Settings): Unit = process(settings) //used by sbt
}

object ILoop {
  implicit def loopToInterpreter(repl: ILoop): IMain = repl.intp

  // Designed primarily for use by test code: take a String with a
  // bunch of code, and prints out a transcript of what it would look
  // like if you'd just typed it into the repl.
  def runForTranscript(code: String, settings: Settings): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val output = new JPrintWriter(new OutputStreamWriter(ostream), true) {
          override def write(str: String) = {
            // completely skip continuation lines
            if (str forall (ch => ch.isWhitespace || ch == '|')) ()
            // print a newline on empty scala prompts
            else if ((str contains '\n') && (str.trim == "scala> ")) super.write("\n")
            else super.write(str)
          }
        }
        val input = new BufferedReader(new StringReader(code)) {
          override def readLine(): String = {
            val s = super.readLine()
            // helping out by printing the line being interpreted.
            if (s != null)
              output.println(s)
            s
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
