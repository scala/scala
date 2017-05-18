package scala.tools.nsc.interpreter.shell

import java.io.PrintWriter

import scala.reflect.internal.util.{NoSourceFile, Position}
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter, Settings}
import scala.tools.nsc.interpreter.{Naming, ReplReporter}
import scala.tools.nsc.reporters.ConsoleReporter


class ReplStrippingWriter(out: PrintWriter) extends PrintWriter(out) {
  // ugh
  var reporter: ReplReporter = _

  def this(replReporter: ReplReporter) = {
    this(replReporter.out)
    reporter = replReporter
  }

  override def write(str: String): Unit =
    if (reporter ne null) super.write(reporter.unmangleInterpreterOutput(str))
    else super.write(str)
}

object ReplReporterImpl {
  def wrapOut(out: PrintWriter) = out match {
    case rsw: ReplStrippingWriter => rsw
    case _ => new ReplStrippingWriter(out)
  }
  def linkRSW(out: PrintWriter, reporter: ReplReporterImpl) =   out match {
    case rsw: ReplStrippingWriter => rsw.reporter = reporter
  }

  val defaultOut = new ReplStrippingWriter(new NewLinePrintWriter(new ConsoleWriter, true))
}
class ReplReporterImpl(val config: ShellConfig, settings: Settings = new Settings, val out: PrintWriter = ReplReporterImpl.defaultOut) extends ConsoleReporter(settings, Console.in, ReplReporterImpl.wrapOut(out)) with ReplReporter  {
  def this(settings: Settings, out: PrintWriter) = this(ShellConfig(settings), settings, out)
  def this(settings: Settings) = this(ShellConfig(settings), settings)

  // TODO remove gross hack -- I don't think the printwriter should be in the business of rewriting output
  // (the unwrapping bit should be moot when we switch to the new repl encoding)
  ReplReporterImpl.linkRSW(writer, this)
  ReplReporterImpl.linkRSW(echoWriter, this)

  def colorOk: Boolean = config.colorOk
  def indentDepth: Int = config.promptText.lines.toList.last.length
  def isDebug: Boolean = config.isReplDebug
  def isTrace: Boolean  = config.isReplTrace

  // whether to print result lines
  var printResults: Boolean = true
  def beQuietDuring[T](body: => T): T = {
    val saved = printResults
    printResults = false
    try body
    finally printResults = saved
  }

  // whether to print anything
  var totalSilence: Boolean = false
  def beSilentDuring[T](operation: => T): T = {
    val saved = totalSilence
    totalSilence = true
    try operation
    finally totalSilence = saved
  }

  /** The maximum length of toString to use when printing the result
    *  of an evaluation.  0 means no maximum.  If a printout requires
    *  more than this number of characters, then the printout is
    *  truncated.
    */
  var maxPrintString = config.maxPrintString.option getOrElse 800

  /** Whether very long lines can be truncated.  This exists so important
    *  debugging information (like printing the classpath) is not rendered
    *  invisible due to the max message length.
    */
  var truncationOK: Boolean = !settings.verbose

  def truncate(str: String): String =
    if (truncationOK && (maxPrintString != 0 && str.length > maxPrintString)) (str take maxPrintString - 3) + "..."
    else str

  def withoutTruncating[T](body: => T): T = {
    val saved = truncationOK
    truncationOK = false
    try body
    finally truncationOK = saved
  }

  /** String unwrapping can be disabled if it is causing issues.
    *  Setting this to false means you will see Strings like "$iw.$iw.".
    */
  var unwrapStrings = true
  def withoutUnwrapping(op: => Unit): Unit = {
    val saved = unwrapStrings
    unwrapStrings = false ; try op finally unwrapStrings = saved
  }
  def unwrap(str: String): String =
    if (unwrapStrings) Naming.unmangle(str)
    else str

  def unmangleInterpreterOutput(str: String): String = truncate(unwrap(str))

  // compiler is initialized
  private[this] var _initializeComplete = false
  def compilerInitialized(): Unit = _initializeComplete = true
  def initializeComplete: Boolean = _initializeComplete

  var currentRequest: scala.tools.nsc.interpreter.IMain#Request = _
  def lastInputSingleLine: Boolean = currentRequest.originalLine.indexOf('\n') == -1
  def preambleLineDelta: Int = currentRequest.preambleEndPos.line

  def printUntruncatedMessage(msg: String): Unit = withoutTruncating(printMessage(msg))

  override def warning(pos: Position, msg: String): Unit = withoutTruncating(super.warning(pos, msg))
  override def error(pos: Position, msg: String): Unit   = withoutTruncating(super.error(pos, msg))

  import scala.io.AnsiColor.{ RED, YELLOW, RESET }

  def severityColor(severity: Severity): String = severity match {
    case ERROR   => RED
    case WARNING => YELLOW
    case INFO    => RESET
  }

  override def print(pos: Position, msg: String, severity: Severity): Unit = {
    val prefix =
      if (colorOk) severityColor(severity) + clabel(severity) + RESET
      else clabel(severity)

    printMessage(pos, prefix + msg)
  }

  private val indentation = " " * indentDepth
  private def indented(str: String) = str.lines.mkString(indentation, "\n" + indentation, "")

  // indent errors, error message uses the caret to point at the line already on the screen instead of repeating it
  // TODO: can we splice the error into the code the user typed when multiple lines were entered?
  // (should also comment out the error to keep multi-line copy/pastable)
  // TODO: multiple errors are not very intuitive (should the second error for same line repeat the line?)
  // TODO: the console could be empty due to external changes (also, :reset? -- see unfortunate example in jvm/interpeter (plusOne))
  override def printMessage(posIn: Position, msg: String): Unit = {
    if ((posIn eq null) || (posIn.source eq NoSourceFile)) printMessage(msg)
    else {
      val consoleLinePrefix = "On line "
      val locationPrefix =
        posIn.source.file.name match { case  "<console>" => consoleLinePrefix case n => s"$n:" }

      // If there's only one line of input, and it's already printed on the console (as indicated by the position's source file name),
      // reuse that line in our error output, and suppress the line number (since we know it's `1`)
      // TODO: this is not perfect, see e.g. test/files/run/repl-colon-type.scala,
      // where the error refers to a line that's not on the screen
      if (locationPrefix == consoleLinePrefix &&
        lastInputSingleLine &&
        posIn.line > preambleLineDelta) { // line won't be printed if it was part of the preamble
        printMessage(indentation + posIn.lineCaret)
        printMessage(indented(msg))
      }
      else {
        val (msgFirstLine, msgRest) =
          msg.indexOf('\n') match {
            case -1 => (msg, "")
            case n => (msg.substring(0, n), msg.substring((n + 1) min msg.length))
          }

        // add newline to get away from prompt when we're reporting on a script/paste
        if (locationPrefix != consoleLinePrefix) printMessage("")

        printMessage(indentation + posIn.lineContent)
        printMessage(indentation + posIn.lineCaret)
        printMessage(s"$locationPrefix${posIn.line - preambleLineDelta}: $msgFirstLine")
        if (!msgRest.isEmpty) printMessage(indented(msgRest))
      }
    }
  }

  override def printMessage(msg: String): Unit = {
    // TODO: deadlock avoidance should not longer be needed (reporter is now eager inside imain)
    // Avoiding deadlock if the compiler starts logging before the lazy val is complete.
    if (initializeComplete) {
      if (totalSilence) {
        if (isTrace)
          super.printMessage("[silent] " + msg)
      }
      else super.printMessage(msg)
    }
    else Console.println("[init] " + msg)
  }

  override def displayPrompt(): Unit =
    if (!totalSilence) super.displayPrompt()

  override def rerunWithDetails(setting: reflect.internal.settings.MutableSettings#Setting, name: String): String =
    s"; for details, enable `:setting $name' or `:replay $name'"

}
