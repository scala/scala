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

// Copyright 2005-2017 LAMP/EPFL and Lightbend, Inc.

package scala.tools.nsc.interpreter
package shell

import java.io.{BufferedReader, PrintWriter}
import java.nio.file.{Files, Path => JPath}
import java.util.concurrent.TimeUnit

import scala.PartialFunction.cond
import scala.Predef.{println => _, _}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.reflect.classTag
import scala.reflect.internal.util.{BatchSourceFile, NoPosition}
import scala.reflect.io.{AbstractFile, Directory, File, Path}
import scala.sys.process.Parser.tokenize
import scala.tools.asm.ClassReader
import scala.tools.nsc.Settings
import scala.tools.nsc.util.{stackTraceString, stringFromStream}
import scala.tools.nsc.interpreter.{AbstractOrMissingHandler, Repl, IMain, Phased, jline}
import scala.tools.nsc.interpreter.Results.{Error, Incomplete, Success}
import scala.tools.nsc.interpreter.StdReplTags._
import scala.util.chaining._

/** The Scala interactive shell. This part provides the user interface,
  * with evaluation and auto-complete handled by IMain.
  *
  * There should be no direct dependency of this code on the compiler;
  * it should all go through the `intp` reference to the interpreter,
  * or maybe eventually even over the wire to a remote compiler.
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

  def global = intp.asInstanceOf[IMain].global

  // Set by run and interpretAllFrom (to read input from file).
  private var in: InteractiveReader = _

  // TODO: the new interface should make settings a ctor arg of ILoop,
  // so that this can be a lazy val
  private lazy val defaultIn: InteractiveReader =
    if (batchMode) SimpleReader(batchText)
    else if (inOverride != null) SimpleReader(inOverride, out, completion(new Accumulator), interactive = true)
    else if (haveInteractiveConsole) {
      val accumulator = new Accumulator
      jline.Reader(config, intp, completion(accumulator), accumulator)
    }
    else SimpleReader()

  private val interpreterInitialized = new java.util.concurrent.CountDownLatch(1)

  def createTempDirectory(): JPath = Files.createTempDirectory("scala-repl").tap(_.toFile().deleteOnExit())

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

  private def printShellInterrupt() = out.print(ShellConfig.InterruptedString)

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
  def closeInterpreter(): Unit = {
    if (intp ne null) {
      intp.close()
      intp = null
    }
    if (in ne null) {
      in.close()
      in = null
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
  lazy val historyCommand = new LoopCommand("history", "show the history (optional num is commands to show)", None) {
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
  def searchHistory(_cmdline: String): Unit = {
    val cmdline = _cmdline.toLowerCase
    val offset  = history.index - history.size + 1

    for ((line, index) <- history.asStrings.zipWithIndex ; if line.toLowerCase contains cmdline)
      echo("%d %s".format(index + offset, line))
  }

  import LoopCommand.{ cmd, nullary, cmdWithHelp }

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    cmd("completions", "<string>", "output completions for the given string", completionsCommand),
    // TODO maybe just drop these commands, as jline subsumes them -- before reenabling, finish scala.tools.nsc.interpreter.jline.HistoryAdaptor
    //cmd("edit", "<id>|<line>", "edit history", editCommand),
    //historyCommand,
    //cmd("h?", "<string>", "search the history", searchHistory),
    cmd("imports", "[name name ...]", "show import history, identifying sources of names", importsCommand),
    cmd("implicits", "[-v]", "show the implicits in scope", implicitsCommand),
    cmd("javap", "<path|class>", "disassemble a file or class name", javapCommand),
    cmd("line", "<id>|<line>", "place line(s) at the end of history", lineCommand),
    cmd("load", "<path>", "interpret lines in a file", loadCommand, fileCompletion),
    cmd("paste", "[-raw] [path]", "enter paste mode or paste a file", pasteCommand, fileCompletion),
    nullary("power", "enable power user mode", () => powerCmd()),
    nullary("quit", "exit the REPL", () => Result(keepRunning = false, None)),
    cmd("replay", "[options]", "reset the REPL and replay all previous commands", replayCommand, settingsCompletion),
    cmd("require", "<path>", "add a jar to the classpath", require),
    cmd("reset", "[options]", "reset the REPL to its initial state, forgetting all session entries", resetCommand, settingsCompletion),
    cmd("save", "<path>", "save replayable session to a file", saveCommand, fileCompletion),
    shCommand,
    cmd("settings", "<options>", "update compiler options, if possible; see reset", changeSettings, settingsCompletion),
    nullary("silent", "disable/enable automatic printing of results", () => verbosity()),
    cmd("type", "[-v] <expr>", "display the type of an expression without evaluating it", typeCommand),
    cmdWithHelp("kind", kindUsage, "display the kind of a type. see also :help kind", Some(kindCommandDetailedHelp), kindCommand),
    nullary("warnings", "show the suppressed warnings from the most recent line which had any", () => warningsCommand())
  )

  /** Power user commands */
  lazy val powerCommands: List[LoopCommand] = List(
    cmd("phase", "<phase>", "set the implicit phase for power commands", phaseCommand)
  )

  // complete filename
  val fileCompletion: Completion = new Completion {
    val emptyWord    = """(\s+)$""".r.unanchored
    val directorily  = """(\S*/)$""".r.unanchored
    val trailingWord = """(\S+)$""".r.unanchored
    def listed(buffer: String, i: Int, dir: Option[Path]) =
      dir.filter(_.isDirectory)
        .map(d => CompletionResult(buffer, i, d.toDirectory.list.map(x => CompletionCandidate(x.name)).toList))
        .getOrElse(NoCompletions)
    def listedIn(dir: Directory, name: String) = dir.list.filter(_.name.startsWith(name)).map(_.name).toList
    def complete(buffer: String, cursor: Int, filter: Boolean): CompletionResult =
      buffer.substring(0, cursor) match {
        case emptyWord(s)        => listed(buffer, cursor, Directory.Current)
        case directorily(s)      => listed(buffer, cursor, Option(Path(s)))
        case trailingWord(s) =>
          val f = File(s)
          val (i, maybes) =
            if (f.isFile) (cursor - s.length, List(f.toAbsolute.path))
            else if (f.isDirectory) (cursor - s.length, List(s"${f.toAbsolute.path}/"))
            else if (f.parent.exists) (cursor - f.name.length, listedIn(f.parent.toDirectory, f.name))
            else (-1, Nil)
          if (maybes.isEmpty) NoCompletions else CompletionResult(buffer, i, maybes.map(CompletionCandidate(_)))
        case _                   => NoCompletions
      }
  }

  // complete settings name
  val settingsCompletion: Completion = new Completion {
    val trailingWord = """(\S+)$""".r.unanchored
    def complete(buffer: String, cursor: Int, filter: Boolean): CompletionResult = {
      buffer.substring(0, cursor) match {
        case trailingWord(s) =>
          val maybes = intp.visibleSettings.filter(x => if (filter) x.name.startsWith(s) else true).map(_.name)
                               .filterNot(cond(_) { case "-"|"-X"|"-Y" => true }).sorted
          if (maybes.isEmpty) NoCompletions
          else CompletionResult(buffer, cursor - s.length, maybes.map(CompletionCandidate(_)), "", "")
        case _ => NoCompletions
      }
    }
  }


  private def importsCommand(line: String): Result =
    intp.importsCommandInternal(words(line)) mkString ("\n")

  private def implicitsCommand(line: String): Result = {
    val (implicits, res) = intp.implicitsCommandInternal(line)
    implicits foreach echoCommandMessage
    res
  }

  // Still todo: modules.
  private def typeCommand(line0: String): Result = {
    line0.trim match {
      case "" => ":type [-v] <expression>. see also :help kind"
      case s  =>
        val verbose = s startsWith "-v "
        val (sig, verboseSig) = intp.typeCommandInternal(s.stripPrefix("-v ").trim, verbose)
        if (verbose) echoCommandMessage("// Type signature")
        echoCommandMessage(sig)
        if (!verboseSig.isEmpty) echoCommandMessage("\n// Internal Type structure\n"+ verboseSig)
        ()
    }
  }

  private lazy val kindUsage: String = "[-v] <type>"

  private lazy val kindCommandDetailedHelp: String =
    s""":kind $kindUsage
       |Displays the kind of a given type.
       |
       |    -v      Displays verbose info.
       |
       |"Kind" is a word used to classify types and type constructors
       |according to their level of abstractness.
       |
       |Concrete, fully specified types such as `Int` and `Option[Int]`
       |are called "proper types" and denoted as `A` using Scala
       |notation, or with the `*` symbol.
       |
       |    scala> :kind Option[Int]
       |    Option[Int]'s kind is A
       |
       |In the above, `Option` is an example of a first-order type
       |constructor, which is denoted as `F[A]` using Scala notation, or
       |* -> * using the star notation. `:kind` also includes variance
       |information in its output, so if we ask for the kind of `Option`,
       |we actually see `F[+A]`:
       |
       |    scala> :k -v Option
       |    Option's kind is F[+A]
       |    * -(+)-> *
       |    This is a type constructor: a 1st-order-kinded type.
       |
       |When you have more complicated types, `:kind` can be used to find
       |out what you need to pass in.
       |
       |    scala> trait ~>[-F1[_], +F2[_]] {}
       |    scala> :kind ~>
       |    ~>'s kind is X[-F1[A1],+F2[A2]]
       |
       |This shows that `~>` accepts something of `F[A]` kind, such as
       |`List` or `Vector`. It's an example of a type constructor that
       |abstracts over type constructors, also known as a higher-order
       |type constructor or a higher-kinded type.
       |""".stripMargin

  private def kindCommand(expr: String): Result = {
    expr.trim match {
      case "" => s":kind $kindUsage"
      case s  => intp.kindCommandInternal(s.stripPrefix("-v ").trim, verbose = s.startsWith("-v "))
    }
  }

  private def warningsCommand(): Result = {
    if (intp.lastWarnings.isEmpty)
      "Can't find any cached warnings."
    else
      intp.lastWarnings foreach { case (pos, msg) => intp.reporter.warning(pos, msg) }
  }

  private def javapCommand(line: String): Result = {
    def handle(results: List[Javap.JpResult]): Result =
      results match {
        case Nil => ()
        case res :: rest =>
          if (res.isError) res.value.toString
          else {
            res.show()
            handle(rest)
          }
      }
    handle(Javap(intp)(words(line): _*))
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

  // Notice failure to create compiler
  def command(line: String): Result =
    if (line startsWith ":") colonCommand(line)
    else if (!intp.initializeCompiler()) Result(keepRunning = false, None)
    else Result(keepRunning = true, interpretStartingWith(line))

  // return false if repl should exit
  def processLine(line: String): Boolean = {
    // Long timeout here to avoid test failures under heavy load.
    interpreterInitialized.await(10, TimeUnit.MINUTES)

    val res = command(line)
    res.lineToRecord.foreach(addReplay)
    res.keepRunning
  }

  lazy val prompt = encolor(promptText)

  // R as in REPL
  def readOneLine(): String = {
    out.flush()
    in.reset()
    in.readLine(prompt)
  }

  // L as in REPL
  @tailrec final def loop(): LineResult =
    readOneLine() match {
      case null => LineResults.EOF
      case s if (try processLine(s) catch crashRecovery) => loop()
      case _    => LineResults.ERR
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
    val intp = this.intp
    def showSettings() = for (s <- { intp.userSetSettings }.toSeq.sorted(Ordering.ordered[intp.Setting])) echo(s.toString)
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
    else {
      val reprompt = "replay> "
      intp.reporter.indenting(reprompt.length) {
        for (cmd <- replayCommands) {
          echo(s"$reprompt$cmd")
          command(cmd)
          echo("") // flush because maybe cmd will have its own output
        }
      }
    }
  }
  /** `reset` the interpreter in an attempt to start fresh.
   *  Supplying settings creates a new compiler.
   */
  def resetCommand(line: String): Unit = {
    def run(destructive: Boolean): Unit = {
      echo("Resetting REPL state.")
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
  def reset(): Unit = {
    intp.reset()
    unleashAndSetPhase()
  }

  def lineCommand(what: String): Result = editCommand(what, None)

  def completion(accumulator: Accumulator = new Accumulator) = {
    val rc = new ReplCompletion(intp, accumulator)
    MultiCompletion(shellCompletion, rc)
  }
  val shellCompletion = new Completion {
    override def complete(buffer: String, cursor: Int, filter: Boolean) =
      if (buffer.startsWith(":")) colonCompletion(buffer, cursor).complete(buffer, cursor, filter)
      else NoCompletions
  }

  // this may be used by editors that embed the REPL (e.g. emacs) to present completions themselves;
  // it's also used by ReplTest
  def completionsCommand(what: String): Result = {
    val completions = in.completion.complete(what, what.length)
    val candidates = completions.candidates.filterNot(_.isUniversal)
    // condition here is a bit weird because of the weird hack we have where
    // the first candidate having an empty defString means it's not really
    // completion, but showing the method signature instead
    if (candidates.headOption.exists(_.name.nonEmpty)) {
      val prefix =
        if (completions == NoCompletions) ""
        else what.substring(0, completions.cursor)
      // hvesalai (emacs sbt-mode maintainer) says it's important to echo only once and not per-line
      echo(
        candidates.map(c => s"[completions] $prefix${c.name}")
          .mkString("\n")
      )
    }
    Result.default // never record completions
  }

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
                  echo(edited.linesIterator map ("+" + _) mkString "\n")
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
  lazy val shCommand = new LoopCommand("sh", "run a shell command (result is implicitly => List[String])", None) {
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
    Some(File(filename)).filter(_.exists).map(action).tap(res =>
      if (res.isEmpty) intp.reporter.warning(NoPosition, s"File `$filename` does not exist.")
    )
  }

  def loadCommand(arg: String): Result = {
    def run(file: String, args: List[String], verbose: Boolean) = withFile(file) { f =>
      intp.interpret(s"val args: Array[String] = ${ args.map("\"" + _ + "\"").mkString("Array(", ",", ")") }")
      interpretAllFrom(f, verbose)
      Result recording s":load $arg"
    } getOrElse Result.default

    tokenize(arg) match {
      case "-v" :: file :: rest => run(file, rest, verbose = true)
      case file :: rest         => run(file, rest, verbose = false)
      case _                    => echo("usage: :load -v file") ; Result.default
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

  def asyncEcho(async: Boolean, msg: => String): Unit = {
    if (async) asyncMessage(msg)
    else echo(msg)
  }

  def verbosity() = {
    intp.reporter.togglePrintResults()
    replinfo(s"Result printing is ${ if (intp.reporter.printResults) "on" else "off" }.")
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
   * and optionally
   * :paste -java
   */
  def pasteCommand(arg: String): Result = {
    var shouldReplay: Option[String] = None
    var label = "<pastie>"
    def result = Result(keepRunning = true, shouldReplay)
    val (flags, args) = tokenize(arg).span(_.startsWith("-"))
    val raw  = flags.contains("-raw")
    val java = flags.contains("-java")
    val badFlags = flags.filterNot(List("-raw", "-java").contains)
    def usage() = echo("usage: :paste [-raw | -java] file | < EOF")
    def pasteFile(name: String): String = {
      label = name
      withFile(name) { f =>
        shouldReplay = Some(s":paste $arg")
        f.slurp().trim().tap(s => echo(if (s.isEmpty) s"File contains no code: $f" else s"Pasting file $f..."))
      }.getOrElse("")
    }
    def pasteWith(margin: String, eof: Option[String]): String = {
      echo(s"// Entering paste mode (${ eof getOrElse "ctrl-D" } to finish)\n")
      in.withSecondaryPrompt("") {
        val delimiter = eof.orElse(config.pasteDelimiter.option)
        def atEOF(s: String) = delimiter.map(_ == s).getOrElse(false)
        val input = readWhile(s => !atEOF(s)).mkString("\n")
        margin match {
          case ""  => input.trim
          case "-" => input.linesIterator.map(_.trim).mkString("\n")
          case _   => input.stripMargin(margin.head).trim
        }
      }
    }
    def interpretCode(code: String) = {
      echo("// Exiting paste mode... now interpreting.")
      if (intp.withLabel(label)(intp.interpret(code)) == Incomplete)
        paste.incomplete("The pasted code is incomplete!", label, code)
    }
    def compileCode(code: String) = {
      echo("// Exiting paste mode... now compiling with scalac.")
      paste.compilePaste(label = label, code = code)
    }
    def compileJava(code: String): Unit = {
      def pickLabel() = {
        val gstable = global
        val jparser = gstable.newJavaUnitParser(gstable.newCompilationUnit(code = code))
        val result = jparser.parse().collect {
          case gstable.ClassDef(mods, className, _, _) if mods.isPublic => className
        }
        result.headOption
      }
      echo("// Exiting paste mode... now compiling with javac.")
      pickLabel() match {
        case Some(className) =>
          label = s"${className.decoded}"
          val out = createTempDirectory()
          JavacTool(out, intp.classLoader).compile(label, code) match {
            case Some(errormsg) => echo(s"Compilation failed! $errormsg")
            case None => intp.addUrlsToClassPath(out.toUri().toURL())
          }
        case _ =>
          echo(s"No class detected in source!")
      }
    }
    def dispatch(code: String): Unit =
      if (code.isEmpty)
        echo("// Exiting paste mode... nothing to compile.")
      else
        intp.reporter.indenting(0) {
          if (java) compileJava(code)
          else if (raw || paste.isPackaged(code)) compileCode(code)
          else interpretCode(code)
        }
    args match {
      case _ if badFlags.nonEmpty                     => usage()
      case name :: Nil if !name.startsWith("<")       => dispatch(pasteFile(name))
      case Nil                                        => dispatch(pasteWith("", None))
      case here :: Nil                                => dispatch(pasteWith(here.slice(1, 2), None))
      case here :: eof :: Nil if here.startsWith("<") => dispatch(pasteWith(here.slice(1, 2), Some(eof)))
      case _                                          => usage()
    }
    result
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
  final def interpretStartingWith(start: String): Option[String] = {
    def loop(): Option[String] = {
      val code = in.accumulator.toString
      intp.interpret(code) match {
        case Error      => None
        case Success    => Some(code)
        case Incomplete if in.interactive && code.endsWith("\n\n") =>
          echo("You typed two blank lines.  Starting a new command.")
          None
        case Incomplete =>
          in.readLine(paste.ContinuePrompt) match {
            case null =>
              // partial input with no input forthcoming,
              // so ask again for parse error message.
              // This happens at EOF of a :load file.
              intp.interpretFinally(code)
              None
            case line => in.accumulator += line ; loop()
          }
      }
    }

    start match {
      case "" | lineComment() => None // empty or line comment, do nothing
      case paste() =>
        val pasted = Iterator(start) ++ readWhile(!paste.isPromptOnly(_))
        paste.transcript(pasted) match {
          case Some(s) => interpretStartingWith(s)
          case _       => None
        }
      case invocation() => in.accumulator += intp.mostRecentVar + start ; loop()
      case _ => in.accumulator += start ; loop()
    }
  }

  /**
   * Allows to specify custom code to run quietly in the preamble
   * @return custom Scala code to run automatically at the startup of the REPL
   */
  protected def internalReplAutorunCode(): Seq[String] = Seq.empty 
  
  /** Actions to cram in parallel while collecting first user input at prompt.
    * Run with output muted both from ILoop and from the intp reporter.
    */
  private def interpretPreamble() = {
    // Bind intp somewhere out of the regular namespace where
    // we can get at it in generated code.
    intp.quietBind(intp.namedParam[Repl](s"$$intp", intp)(tagOfRepl, classTag[Repl]))

    internalReplAutorunCode().foreach(intp.quietRun)
    
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
  def run(interpreterSettings: Settings): Boolean = {
    if (!batchMode) printWelcome()

    createInterpreter(interpreterSettings)
    in = defaultIn

    intp.reporter.withoutPrintingResults(intp.withSuppressedSettings {
      intp.initializeCompiler()
      interpreterInitialized.countDown() // TODO: move to reporter.compilerInitialized ?

      if (intp.reporter.hasErrors) {
        echo("Interpreter encountered errors during initialization!")
        throw new InterruptedException
      }

      echoOff { interpretPreamble() }
    })

    // start full loop (if initialization was successful)
    try
      loop() match {
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

  class TestConfig(delegate: ShellConfig) extends ShellConfig {
    def filesToPaste: List[String] = delegate.filesToPaste
    def filesToLoad: List[String] = delegate.filesToLoad
    def batchText: String = delegate.batchText
    def batchMode: Boolean = delegate.batchMode
    def doCompletion: Boolean = delegate.doCompletion
    def haveInteractiveConsole: Boolean = delegate.haveInteractiveConsole

    def xsource: String = ""

    override val colorOk = delegate.colorOk

    // No truncated output, because the result changes on Windows because of line endings
    override val maxPrintString = sys.Prop[Int]("wtf").tap(_.set("0"))
  }
  object TestConfig {
    def apply(settings: Settings) = new TestConfig(ShellConfig(settings))
  }

  // Designed primarily for use by test code: take a String with a
  // bunch of code, and prints out a transcript of what it would look
  // like if you'd just typed it into the repl.
  def runForTranscript(code: String, settings: Settings, inSession: Boolean = false): String =
    runForTranscript(code, settings, TestConfig(settings), inSession)

  def runForTranscript(code: String, settings: Settings, config: ShellConfig, inSession: Boolean): String = {
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
  def run(lines: List[String]): String = run(lines.mkString("", "\n", "\n"))
}
