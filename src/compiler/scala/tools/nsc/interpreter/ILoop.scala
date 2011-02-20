/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import java.io.{ BufferedReader, FileReader, PrintWriter }

import scala.sys.process.Process
import session._
import scala.tools.nsc.interpreter.{ Results => IR }
import scala.tools.util.{ SignalManager, Signallable, Javap }
import scala.annotation.tailrec
import scala.util.control.Exception.{ ignoring }
import scala.collection.mutable.ListBuffer
import scala.concurrent.ops
import util.{ ClassPath, Exceptional, stringFromWriter, stringFromStream }
import interpreter._
import io.{ File, Sources }

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
class ILoop(in0: Option[BufferedReader], protected val out: PrintWriter)
                extends AnyRef
                   with LoopCommands
{
  def this(in0: BufferedReader, out: PrintWriter) = this(Some(in0), out)
  def this() = this(None, new PrintWriter(Console.out))

  var in: InteractiveReader = _   // the input stream from which commands come
  var settings: Settings = _
  var intp: IMain = _
  var power: Power = _

  // TODO
  // object opt extends AestheticSettings

  @deprecated("Use `intp` instead.")
  def interpreter = intp

  @deprecated("Use `intp` instead.")
  def interpreter_= (i: Interpreter): Unit = intp = i

  def history = in.history

  /** The context class loader at the time this object was created */
  protected val originalClassLoader = Thread.currentThread.getContextClassLoader

  // Install a signal handler so we can be prodded.
  private val signallable =
    if (isReplDebug) Signallable("Dump repl state.")(dumpCommand(""))
    else null

  // classpath entries added via :cp
  var addedClasspath: String = ""

  /** A reverse list of commands to replay if the user requests a :replay */
  var replayCommandStack: List[String] = Nil

  /** A list of commands to replay if the user requests a :replay */
  def replayCommands = replayCommandStack.reverse

  /** Record a command for replay should the user request a :replay */
  def addReplay(cmd: String) = replayCommandStack ::= cmd

  /** Try to install sigint handler: ignore failure.  Signal handler
   *  will interrupt current line execution if any is in progress.
   *
   *  Attempting to protect the repl from accidental exit, we only honor
   *  a single ctrl-C if the current buffer is empty: otherwise we look
   *  for a second one within a short time.
   */
  private def installSigIntHandler() {
    def onExit() {
      Console.println("") // avoiding "shell prompt in middle of line" syndrome
      sys.exit(1)
    }
    ignoring(classOf[Exception]) {
      SignalManager("INT") = {
        if (intp == null)
          onExit()
        else if (intp.lineManager.running)
          intp.lineManager.cancel()
        else if (in.currentLine != "") {
          // non-empty buffer, so make them hit ctrl-C a second time
          SignalManager("INT") = onExit()
          io.timer(5)(installSigIntHandler())  // and restore original handler if they don't
        }
        else onExit()
      }
    }
  }

  /** Close the interpreter and set the var to null. */
  def closeInterpreter() {
    if (intp ne null) {
      intp.close
      intp = null
      power = null
      Thread.currentThread.setContextClassLoader(originalClassLoader)
    }
  }

  class ILoopInterpreter extends IMain(settings, out) {
    override lazy val formatting = new Formatting {
      def prompt = ILoop.this.prompt
    }
    override protected def createLineManager() = new Line.Manager {
      override def onRunaway(line: Line[_]): Unit = {
        val template = """
          |// She's gone rogue, captain! Have to take her out!
          |// Calling Thread.stop on runaway %s with offending code:
          |// scala> %s""".stripMargin

        println(template.format(line.thread, line.code))
        // XXX no way to suppress the deprecation warning
        line.thread.stop()
        in.redrawLine()
      }
    }
    override protected def parentClassLoader =
      settings.explicitParentLoader.getOrElse( classOf[ILoop].getClassLoader )
  }

  /** Create a new interpreter. */
  def createInterpreter() {
    if (addedClasspath != "")
      settings.classpath append addedClasspath

    intp = new ILoopInterpreter
    intp.setContextClassLoader()
    installSigIntHandler()
  }

  /** print a friendly help message */
  def printHelp() = {
    out println "All commands can be abbreviated - for example :he instead of :help.\n"
    val cmds = commands map (x => (x.usage, x.help))
    val width: Int = cmds map { case (x, _) => x.length } max
    val formatStr = "%-" + width + "s %s"
    cmds foreach { case (usage, help) => out println formatStr.format(usage, help) }
  }

  /** Print a welcome message */
  def printWelcome() {
    import Properties._
    val welcomeMsg =
     """|Welcome to Scala %s (%s, Java %s).
        |Type in expressions to have them evaluated.
        |Type :help for more information.""" .
    stripMargin.format(versionString, javaVmName, javaVersion)

    plushln(welcomeMsg)
  }

  /** Show the history */
  def printHistory(xs: List[String]): Result = {
    if (history eq NoHistory)
      return "No history available."

    val defaultLines = 20
    val current = history.index
    val count   = try xs.head.toInt catch { case _: Exception => defaultLines }
    val lines   = history.asStrings takeRight count
    val offset  = current - lines.size + 1

    for ((line, index) <- lines.zipWithIndex)
      println("%3d  %s".format(index + offset, line))
  }

  /** Some print conveniences */
  def println(x: Any) = out println x
  def plush(x: Any)   = { out print x ; out.flush() }
  def plushln(x: Any) = { out println x ; out.flush() }

  /** Search the history */
  def searchHistory(_cmdline: String) {
    val cmdline = _cmdline.toLowerCase
    val offset  = history.index - history.size + 1

    for ((line, index) <- history.asStrings.zipWithIndex ; if line.toLowerCase contains cmdline)
      println("%d %s".format(index + offset, line))
  }

  private var currentPrompt = Properties.shellPromptString
  def setPrompt(prompt: String) = currentPrompt = prompt
  /** Prompt to print when awaiting input */
  def prompt = currentPrompt

  /** Standard commands **/
  val standardCommands: List[LoopCommand] = {
    List(
       LineArg("cp", "add an entry (jar or directory) to the classpath", addClasspath),
       NoArgs("help", "print this help message", printHelp),
       VarArgs("history", "show the history (optional arg: lines to show)", printHistory),
       LineArg("h?", "search the history", searchHistory),
       LineArg("implicits", "show the implicits in scope (-v to include Predef)", implicitsCommand),
       LineArg("javap", "disassemble a file or class name", javapCommand),
       LineArg("keybindings", "show how ctrl-[A-Z] and other keys are bound", keybindingsCommand),
       OneArg("load", "load and interpret a Scala file", load),
       NoArgs("power", "enable power user mode", powerCmd),
       NoArgs("quit", "exit the interpreter", () => Result(false, None)),
       NoArgs("replay", "reset execution and replay all previous commands", replay),
       LineArg("sh", "fork a shell and run a command", shCommand),
       NoArgs("silent", "disable/enable automatic printing of results", verbosity)
    )
  }

  /** Power user commands */
  val powerCommands: List[LoopCommand] = {
    List(
      LineArg("dump", "displays a view of the interpreter's internal state", dumpCommand),
      LineArg("phase", "set the implicit phase for power commands", phaseCommand),
      LineArg("symfilter", "change the filter for symbol printing", symfilterCmd),
      LineArg("wrap", "code to wrap around all executions", wrapCommand)
    )
  }

  private def dumpCommand(line: String): Result = {
    println(power)
    history.asStrings takeRight 30 foreach println
    in.redrawLine()
  }

  private val typeTransforms = List(
    "scala.collection.immutable." -> "immutable.",
    "scala.collection.mutable." -> "mutable.",
    "scala.collection.generic." -> "generic.",
    "java.lang." -> "jl.",
    "scala.runtime." -> "runtime."
  )

  private def implicitsCommand(line: String): Result = {
    val intp = ILoop.this.intp
    import intp._
    import global.Symbol

    def p(x: Any) = intp.reporter.printMessage("" + x)

    // If an argument is given, only show a source with that
    // in its name somewhere.
    val args     = line split "\\s+"
    val filtered = intp.implicitSymbolsBySource filter {
      case (source, syms) =>
        (args contains "-v") || {
          if (line == "") (source.fullName.toString != "scala.Predef")
          else (args exists (source.name.toString contains _))
        }
    }

    if (filtered.isEmpty)
      return "No implicits have been imported other than those in Predef."

    filtered foreach {
      case (source, syms) =>
        p("/* " + syms.size + " implicit members imported from " + source.fullName + " */")

        // This groups the members by where the symbol is defined
        val byOwner = syms groupBy (_.owner)
        val sortedOwners = byOwner.toList sortBy { case (owner, _) => intp.afterTyper(source.info.baseClasses indexOf owner) }

        sortedOwners foreach {
          case (owner, members) =>
            // Within each owner, we cluster results based on the final result type
            // if there are more than a couple, and sort each cluster based on name.
            // This is really just trying to make the 100 or so implicits imported
            // by default into something readable.
            val memberGroups: List[List[Symbol]] = {
              val groups = members groupBy (_.tpe.finalResultType) toList
              val (big, small) = groups partition (_._2.size > 3)
              val xss = (
                (big sortBy (_._1.toString) map (_._2)) :+
                (small flatMap (_._2))
              )

              xss map (xs => xs sortBy (_.name.toString))
            }

            val ownerMessage = if (owner == source) " defined in " else " inherited from "
            p("  /* " + members.size + ownerMessage + owner.fullName + " */")

            memberGroups foreach { group =>
              group foreach (s => p("  " + intp.symbolDefString(s)))
              p("")
            }
        }
        p("")
    }
  }

  private object javap extends Javap(intp.classLoader, new IMain.ReplStrippingWriter(intp)) {
    override def tryClass(path: String): Array[Byte] = {
      // Look for Foo first, then Foo$, but if Foo$ is given explicitly,
      // we have to drop the $ to find object Foo, then tack it back onto
      // the end of the flattened name.
      def className  = intp pathToFlatName path
      def moduleName = (intp pathToFlatName path.stripSuffix("$")) + "$"

      val bytes = super.tryClass(className)
      if (bytes.nonEmpty) bytes
      else super.tryClass(moduleName)
    }
  }

  private def javapCommand(line: String): Result = {
    if (line == "")
      return ":javap [-lcsvp] [path1 path2 ...]"

    javap(words(line)) foreach { res =>
      if (res.isError) return "Failed: " + res.value
      else res.show()
    }
  }
  private def keybindingsCommand(line: String): Result = {
    if (in.keyBindings.isEmpty) "Key bindings unavailable."
    else {
      println("Reading jline properties for default key bindings.")
      println("Accuracy not guaranteed: treat this as a guideline only.\n")
      in.keyBindings foreach println
    }
  }
  private def wrapCommand(line: String): Result = {
    intp setExecutionWrapper line
    if (line == "") "Cleared wrapper."
    else "Set wrapper to '" + line + "'"
  }

  private def symfilterCmd(line: String): Result = {
    if (line == "") {
      power.vars.symfilter set "_ => true"
      "Remove symbol filter."
    }
    else {
      power.vars.symfilter set line
      "Set symbol filter to '" + line + "'."
    }
  }
  private def pathToPhased = intp.pathToTerm("power") + ".phased"
  private def phaseCommand(name: String): Result = {
    // This line crashes us in TreeGen:
    //
    //   if (intp.power.phased set name) "..."
    //
    // Exception in thread "main" java.lang.AssertionError: assertion failed: ._7.type
    //  at scala.Predef$.assert(Predef.scala:99)
    //  at scala.tools.nsc.ast.TreeGen.mkAttributedQualifier(TreeGen.scala:69)
    //  at scala.tools.nsc.ast.TreeGen.mkAttributedQualifier(TreeGen.scala:44)
    //  at scala.tools.nsc.ast.TreeGen.mkAttributedRef(TreeGen.scala:101)
    //  at scala.tools.nsc.ast.TreeGen.mkAttributedStableRef(TreeGen.scala:143)
    //
    // But it works like so, type annotated.
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
        intp.setExecutionWrapper(pathToPhased)
        val activeMessage =
          if (what.toString.length == name.length) "" + what
          else "%s (%s)".format(what, name)

        "Active phase is now: " + activeMessage
      }
    }
  }

  /** Available commands */
  def commands: List[LoopCommand] = standardCommands ++ (
    if (power == null) Nil
    else powerCommands
  )

  val replayQuestionMessage =
    """|The repl compiler has crashed spectacularly. Shall I replay your
       |session? I can re-run all lines except the last one.
       |[y/n]
    """.trim.stripMargin

  private val crashRecovery: PartialFunction[Throwable, Unit] = {
    case ex: Throwable =>
      if (settings.YrichExes.value) {
        val sources = implicitly[Sources]
        out.println("\n" + ex.getMessage)
        out.println(
          if (isReplDebug) "[searching " + sources.path + " for exception contexts...]"
          else "[searching for exception contexts...]"
        )
        out.println(Exceptional(ex).force().context())
      }
      else {
        out.println(util.stackTraceString(ex))
      }
      ex match {
        case _: NoSuchMethodError | _: NoClassDefFoundError =>
          out.println("Unrecoverable error.")
          throw ex
        case _  =>
          out.print(replayQuestionMessage)
          out.flush()
          if (in.readAssumingNo("")) {
            out.println("\nAttempting session recovery with replay.")
            replay()
          }
          else out.println("\nAbandoning crashed session.")
      }
  }

  /** The main read-eval-print loop for the repl.  It calls
   *  command() for each line of input, and stops when
   *  command() returns false.
   */
  def loop() {
    def readOneLine() = {
      out.flush
      in readLine prompt
    }
    // return false if repl should exit
    def processLine(line: String): Boolean =
      if (line eq null) false               // assume null means EOF
      else command(line) match {
        case Result(false, _)           => false
        case Result(_, Some(finalLine)) => addReplay(finalLine) ; true
        case _                          => true
      }

    while (true) {
      try if (!processLine(readOneLine)) return
      catch crashRecovery
    }
  }

  /** interpret all lines from a specified file */
  def interpretAllFrom(file: File) {
    val oldIn = in
    val oldReplay = replayCommandStack

    try file applyReader { reader =>
      in = SimpleReader(reader, out, false)
      plushln("Loading " + file + "...")
      loop()
    }
    finally {
      in = oldIn
      replayCommandStack = oldReplay
    }
  }

  /** create a new interpreter and replay all commands so far */
  def replay() {
    closeInterpreter()
    createInterpreter()
    for (cmd <- replayCommands) {
      plushln("Replaying: " + cmd)  // flush because maybe cmd will have its own output
      command(cmd)
      out.println
    }
  }

  /** fork a shell and run a command */
  def shCommand(cmd: String): Result = {
    if (cmd == "")
      return "Usage: sh <command line>"

    intp quietRun "import _root_.scala.sys.process._"
    val pb = Process(cmd)
    intp.bind("builder", pb)
    val stdout = Process(cmd).lines
    intp.bind("stdout", stdout)
    ()
  }

  def withFile(filename: String)(action: File => Unit) {
    val f = File(filename)

    if (f.exists) action(f)
    else out.println("That file does not exist")
  }

  def load(arg: String) = {
    var shouldReplay: Option[String] = None
    withFile(arg)(f => {
      interpretAllFrom(f)
      shouldReplay = Some(":load " + arg)
    })
    Result(true, shouldReplay)
  }

  def addClasspath(arg: String): Unit = {
    val f = File(arg).normalize
    if (f.exists) {
      addedClasspath = ClassPath.join(addedClasspath, f.path)
      val totalClasspath = ClassPath.join(settings.classpath.value, addedClasspath)
      println("Added '%s'.  Your new classpath is:\n\"%s\"".format(f.path, totalClasspath))
      replay()
    }
    else out.println("The path '" + f + "' doesn't seem to exist.")
  }

  def powerCmd(): Result = {
    if (power != null)
      return "Already in power mode."

    power = new Power(this)
    isReplPower = true
    power.unleash()
    power.banner
  }

  def verbosity() = {
    val old = intp.printResults
    intp.printResults = !old
    out.println("Switched " + (if (old) "off" else "on") + " result printing.")
  }

  /** Run one command submitted by the user.  Two values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): Result = {
    def withError(msg: String) = {
      out println msg
      Result(true, None)
    }
    def ambiguous(cmds: List[LoopCommand]) = "Ambiguous: did you mean " + cmds.map(":" + _.name).mkString(" or ") + "?"

    // not a command
    if (!line.startsWith(":")) {
      // Notice failure to create compiler
      if (intp.global == null) return Result(false, None)
      else return Result(true, interpretStartingWith(line))
    }

    val tokens = (line drop 1 split """\s+""").toList
    if (tokens.isEmpty)
      return withError(ambiguous(commands))

    val (cmd :: args) = tokens

    // this lets us add commands willy-nilly and only requires enough command to disambiguate
    commands.filter(_.name startsWith cmd) match {
      case List(x)  => x(args)
      case Nil      => withError("Unknown command.  Type :help for help.")
      case xs       =>
        xs find (_.name == cmd) match {
          case Some(exact)  => exact(args)
          case _            => withError(ambiguous(xs))
        }
    }
  }

  private val CONTINUATION_STRING = "     | "
  private val PROMPT_STRING = "scala> "

  /** If it looks like they're pasting in a scala interpreter
   *  transcript, remove all the formatting we inserted so we
   *  can make some sense of it.
   */
  private var pasteStamp: Long = 0

  /** Returns true if it's long enough to quit. */
  def updatePasteStamp(): Boolean = {
    /* Enough milliseconds between readLines to call it a day. */
    val PASTE_FINISH = 1000

    val prevStamp = pasteStamp
    pasteStamp = System.currentTimeMillis

    (pasteStamp - prevStamp > PASTE_FINISH)

  }
  /** TODO - we could look for the usage of resXX variables in the transcript.
   *  Right now backreferences to auto-named variables will break.
   */

  /** The trailing lines complication was an attempt to work around the introduction
   *  of newlines in e.g. email messages of repl sessions.  It doesn't work because
   *  an unlucky newline can always leave you with a syntactically valid first line,
   *  which is executed before the next line is considered.  So this doesn't actually
   *  accomplish anything, but I'm leaving it in case I decide to try harder.
   */
  case class PasteCommand(cmd: String, trailing: ListBuffer[String] = ListBuffer[String]())

  /** Commands start on lines beginning with "scala>" and each successive
   *  line which begins with the continuation string is appended to that command.
   *  Everything else is discarded.  When the end of the transcript is spotted,
   *  all the commands are replayed.
   */
  @tailrec private def cleanTranscript(lines: List[String], acc: List[PasteCommand]): List[PasteCommand] = lines match {
    case Nil                                    => acc.reverse
    case x :: xs if x startsWith PROMPT_STRING  =>
      val first = x stripPrefix PROMPT_STRING
      val (xs1, xs2) = xs span (_ startsWith CONTINUATION_STRING)
      val rest = xs1 map (_ stripPrefix CONTINUATION_STRING)
      val result = (first :: rest).mkString("", "\n", "\n")

      cleanTranscript(xs2, PasteCommand(result) :: acc)

    case ln :: lns =>
      val newacc = acc match {
        case Nil => Nil
        case PasteCommand(cmd, trailing) :: accrest =>
          PasteCommand(cmd, trailing :+ ln) :: accrest
      }
      cleanTranscript(lns, newacc)
  }

  /** The timestamp is for safety so it doesn't hang looking for the end
   *  of a transcript.  Ad hoc parsing can't be too demanding.  You can
   *  also use ctrl-D to start it parsing.
   */
  @tailrec private def interpretAsPastedTranscript(lines: List[String]) {
    val line = in.readLine("")
    val finished = updatePasteStamp()

    if (line == null || finished || line.trim == PROMPT_STRING.trim) {
      val xs = cleanTranscript(lines.reverse, Nil)
      println("Replaying %d commands from interpreter transcript." format xs.size)
      for (PasteCommand(cmd, trailing) <- xs) {
        out.flush()
        def runCode(code: String, extraLines: List[String]) {
          (intp interpret code) match {
            case IR.Incomplete if extraLines.nonEmpty =>
              runCode(code + "\n" + extraLines.head, extraLines.tail)
            case _ => ()
          }
        }
        runCode(cmd, trailing.toList)
      }
    }
    else
      interpretAsPastedTranscript(line :: lines)
  }

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
            out.println("You typed two blank lines.  Starting a new command.")
            None
          }
          else in.readLine(CONTINUATION_STRING) match {
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
    else if (code startsWith PROMPT_STRING) {
      updatePasteStamp()
      interpretAsPastedTranscript(List(code))
      None
    }
    else if (Completion.looksLikeInvocation(code) && intp.mostRecentVar != "") {
      interpretStartingWith(intp.mostRecentVar + code)
    }
    else {
      def runCompletion = in.completion execute code map (intp bindValue _)
      /** Due to my accidentally letting file completion execution sneak ahead
       *  of actual parsing this now operates in such a way that the scala
       *  interpretation always wins.  However to avoid losing useful file
       *  completion I let it fail and then check the others.  So if you
       *  type /tmp it will echo a failure and then give you a Directory object.
       *  It's not pretty: maybe I'll implement the silence bits I need to avoid
       *  echoing the failure.
       */
      if (intp isParseable code) {
        val (code, result) = reallyInterpret
        if (power != null && code == IR.Error)
          runCompletion

        result
      }
      else runCompletion match {
        case Some(_)  => None // completion hit: avoid the latent error
        case _        => reallyInterpret._2  // trigger the latent error
      }
    }
  }

  // runs :load `file` on any files passed via -i
  def loadFiles(settings: Settings) = settings match {
    case settings: GenericRunnerSettings =>
      for (filename <- settings.loadfiles.value) {
        val cmd = ":load " + filename
        command(cmd)
        addReplay(cmd)
        out.println()
      }
    case _ =>
  }

  /** Tries to create a JLineReader, falling back to SimpleReader:
   *  unless settings or properties are such that it should start
   *  with SimpleReader.
   */
  def chooseReader(settings: Settings): InteractiveReader = {
    if (settings.Xnojline.value || Properties.isEmacsShell)
      SimpleReader()
    else try JLineReader(
      if (settings.noCompletion.value) NoCompletion
      else new JLineCompletion(intp)
    )
    catch {
      case _: Exception | _: NoClassDefFoundError => SimpleReader()
    }
  }

  def process(settings: Settings): Boolean = {
    this.settings = settings
    createInterpreter()

    // sets in to some kind of reader depending on environmental cues
    in = in0 match {
      case Some(reader) => SimpleReader(reader, out, true)
      case None         => chooseReader(settings)
    }

    loadFiles(settings)
    // it is broken on startup; go ahead and exit
    if (intp.reporter.hasErrors)
      return false

    printWelcome()
    try {
      // this is about the illusion of snappiness.  We call initialize()
      // which spins off a separate thread, then print the prompt and try
      // our best to look ready.  Ideally the user will spend a
      // couple seconds saying "wow, it starts so fast!" and by the time
      // they type a command the compiler is ready to roll.
      intp.initialize()
      if (isReplPower) {
        plushln("Starting in power mode, one moment...\n")
        powerCmd()
      }
      loop()
    }
    finally closeInterpreter()
    true
  }

  /** process command-line arguments and do as they request */
  def process(args: Array[String]): Boolean = {
    def error1(msg: String) = out println ("scala: " + msg)
    val command = new CommandLine(args.toList, error1)
    def neededHelp(): String =
      (if (command.settings.help.value) command.usageMsg + "\n" else "") +
      (if (command.settings.Xhelp.value) command.xusageMsg + "\n" else "")

    // if they asked for no help and command is valid, we call the real main
    neededHelp() match {
      case ""     => if (command.ok) main(command.settings) // else nothing
      case help   => plush(help)
    }
    true
  }

  @deprecated("Use `process` instead")
  def main(args: Array[String]): Unit = process(args)
  @deprecated("Use `process` instead")
  def main(settings: Settings): Unit = process(settings)
}

object ILoop {
  implicit def loopToInterpreter(repl: ILoop): IMain = repl.intp

  /** Creates an interpreter loop with default settings and feeds
   *  the given code to it as input.
   */
  def run(code: String): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val input    = new BufferedReader(new StringReader(code))
        val output   = new PrintWriter(new OutputStreamWriter(ostream))
        val repl     = new ILoop(input, output)
        val settings = new Settings
        settings.classpath.value = sys.props("java.class.path")

        repl main settings
      }
    }
  }
  def run(lines: List[String]): String = run(lines map (_ + "\n") mkString)

  // provide the enclosing type T
  // in order to set up the interpreter's classpath and parent class loader properly
  def breakIf[T: Manifest](assertion: => Boolean, args: NamedParam*): Unit =
    if (assertion) break[T](args.toList)

  // start a repl, binding supplied args
  def break[T: Manifest](args: List[NamedParam]): Unit = {
    val msg = if (args.isEmpty) "" else "  Binding " + args.size + " value%s.".format(
      if (args.size == 1) "" else "s"
    )
    Console.println("Debug repl starting." + msg)
    val repl = new ILoop {
      override def prompt = "\ndebug> "
    }
    repl.settings = new Settings(Console println _)
    repl.settings.embeddedDefaults[T]
    repl.createInterpreter()
    repl.in = JLineReader(repl)

    // rebind exit so people don't accidentally call sys.exit by way of predef
    repl.quietRun("""def exit = println("Type :quit to resume program execution.")""")
    args foreach (p => repl.bind(p.name, p.tpe, p.value))
    repl.loop()

    Console.println("\nDebug repl exiting.")
    repl.closeInterpreter()
  }
}
