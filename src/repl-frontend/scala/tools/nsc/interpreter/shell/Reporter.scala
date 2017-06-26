package scala.tools.nsc.interpreter.shell

import java.io.PrintWriter

import scala.reflect.internal.util.{NoSourceFile, Position, StringOps}
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter, Settings}
import scala.tools.nsc.interpreter.{Naming, Repl, ReplReporter, ReplRequest}
import scala.tools.nsc.reporters.AbstractReporter


object ReplReporterImpl {
  val defaultOut = new NewLinePrintWriter(new ConsoleWriter, true)
}

// settings are for AbstractReporter (noWarnings, isVerbose, isDebug)
class ReplReporterImpl(val config: ShellConfig, val settings: Settings = new Settings, writer: PrintWriter = ReplReporterImpl.defaultOut) extends AbstractReporter with ReplReporter  {
  def this(settings: Settings, writer: PrintWriter) = this(ShellConfig(settings), settings, writer)
  def this(settings: Settings) = this(ShellConfig(settings), settings)

  val out: PrintWriter = new ReplStrippingWriter(writer)
  private class ReplStrippingWriter(out: PrintWriter) extends PrintWriter(out) {
    override def write(str: String): Unit =
      super.write(unmangleInterpreterOutput(str))
  }

  override def flush() = out.flush()

  // removes trailing space (for clean partest check files?)
  private def printlnAndFlush(msg: String): Unit = {
    out.println(StringOps.trimAllTrailingSpace(msg))
    flush()
  }

  def colorOk: Boolean = config.colorOk
  def indentDepth: Int = config.promptText.lines.toList.last.length
  def isDebug: Boolean = config.isReplDebug
  def isTrace: Boolean = config.isReplTrace

  var printResults: Boolean = true
  override def togglePrintResults: Unit = printResults = !printResults
  def withoutPrintingResults[T](body: => T): T = {
    val saved = printResults
    printResults = false
    try body
    finally printResults = saved
  }

  override def printResult(result: Either[String, String]): Unit =
    result match {
      case Right(success) =>
        if (!success.isEmpty && printResults)
          printMessage(success stripSuffix "\n") // TODO: can we avoid having to strip the trailing "\n"?
        else if (isDebug) // show quiet-mode activity
          printMessage(success.trim.lines map ("[quiet] " + _) mkString "\n")

      case Left(error) =>
        // don't truncate stack traces
        withoutTruncating { printMessage(error) }
    }


  // whether to print anything
  var totalSilence: Boolean = false
  def suppressOutput[T](operation: => T): T = {
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

  var currentRequest: ReplRequest = _

  def printUntruncatedMessage(msg: String): Unit = withoutTruncating(printMessage(msg))

  override def warning(pos: Position, msg: String): Unit = withoutTruncating(super.warning(pos, msg))
  override def error(pos: Position, msg: String): Unit   = withoutTruncating(super.error(pos, msg))

  import scala.io.AnsiColor.{ RED, YELLOW, RESET }

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => ""
  }

  protected def clabel(severity: Severity): String = label(severity) match {
    case "" => ""
    case s  => s"$s: "
  }

  def severityColor(severity: Severity): String = severity match {
    case ERROR   => RED
    case WARNING => YELLOW
    case INFO    => RESET
  }

  def print(pos: Position, msg: String, severity: Severity): Unit = {
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
  def printMessage(posIn: Position, msg: String): Unit = {
    if ((posIn eq null) || (posIn.source eq NoSourceFile)) printMessage(msg)
    else if (posIn.source.file.name == "<console>" && posIn.line == 1 && posIn.source.content.indexOf("\n") == -1) {
      // If there's only one line of input, and it's already printed on the console (as indicated by the position's source file name),
      // reuse that line in our error output, and suppress the line number (since we know it's `1`)
      // NOTE: see e.g. test/files/run/repl-colon-type.scala, where the error refers to a line that's not on the screen
      printMessage(indentation + posIn.lineCaret)
      printMessage(indented(msg))
    } else {
      // note the side-effect -- don't move this around
      val locationPrefix =
        posIn.source.file.name match {
          case "<console>" => s"On line ${posIn.line}: "
          case n =>
            // add newline to get away from prompt when we're reporting on a script/paste
            printMessage("")
            s"$n:${posIn.line}: "
        }

      val isSynthetic = posIn.source.file.name == "<synthetic>"

      // for errors in synthetic code, don't remove wrapping so we can see what's really going on
      def printLineContent = printMessage(indentation + posIn.lineContent)
      if (isSynthetic) withoutUnwrapping(printLineContent) else printLineContent

      printMessage(indentation + posIn.lineCaret)

      msg.indexOf('\n') match {
        case -1 => printMessage(s"$locationPrefix$msg")
        case n =>
          val msgFirstLine = msg.substring(0, n)
          val msgRest = msg.substring((n + 1) min msg.length)
          printMessage(s"$locationPrefix$msgFirstLine")
          printMessage(indented(msgRest))
      }

      if (isSynthetic) printMessage("\n(To diagnose errors in synthetic code, try adding `// show` to the end of your input.)")
    }
  }

  def printMessage(msg: String): Unit =
    if (!totalSilence) printlnAndFlush(msg)
    else if (isTrace) printlnAndFlush("[silent] " + msg)

  override def displayPrompt(): Unit =
    if (!totalSilence) {
      out.println()
      out.print("a)bort, s)tack, r)esume: ")
      out.flush()
      Console.in.read match { // TODO: use repl reader?
        case 'a' | 'A' =>
          new Throwable().printStackTrace(out)
          System.exit(1)
        case 's' | 'S' =>
          new Throwable().printStackTrace(out)
          out.println()
          out.flush()
        case _ =>
      }
    }

  override def rerunWithDetails(setting: reflect.internal.settings.MutableSettings#Setting, name: String): String =
    s"; for details, enable `:setting $name' or `:replay $name'"

  def display(pos: Position, msg: String, severity: Severity): Unit = {
    val ok = severity match {
      case ERROR   => ERROR.count   <= settings.maxerrs.value
      case WARNING => WARNING.count <= settings.maxwarns.value
      case _     => true
    }
    if (ok) print(pos, msg, severity)
  }

  override def finish() =
    for (k <- List(WARNING, ERROR) if k.count > 0)
      printMessage(s"${StringOps.countElementsAsString(k.count, label(k))} found")

}
