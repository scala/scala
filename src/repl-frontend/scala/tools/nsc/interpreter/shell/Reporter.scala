/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.shell

import java.io.PrintWriter
import scala.reflect.internal
import scala.reflect.internal.util.{CodeAction, NoSourceFile, Position, StringOps}
import scala.tools.nsc.interpreter.{Naming, ReplReporter, ReplRequest}
import scala.tools.nsc.reporters.FilteringReporter
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter, Settings}

object ReplReporterImpl {
  val defaultOut = new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)
}

// settings are for AbstractReporter (noWarnings, isVerbose, isDebug)
class ReplReporterImpl(val config: ShellConfig, val settings: Settings = new Settings, writer: PrintWriter = ReplReporterImpl.defaultOut) extends FilteringReporter with ReplReporter {
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

  private def indentDepth: Int = config.promptText.linesIterator.toList.last.length
  private[this] var indentation: String = " " * indentDepth
  def indenting(n: Int)(body: => Unit): Unit = {
    val save = indentation
    indentation = " " * n
    try body finally indentation = save
  }
  private def indented(str: String) = str.linesIterator.mkString(indentation, "\n" + indentation, "")

  def colorOk: Boolean = config.colorOk
  def isDebug: Boolean = config.isReplDebug
  def isTrace: Boolean = config.isReplTrace

  var printResults: Boolean = true
  override def togglePrintResults(): Unit = printResults = !printResults
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
          printMessage(success.trim.linesIterator map ("[quiet] " + _) mkString "\n")

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
  var truncationOK: Boolean = !settings.verbose.value

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
   *  Setting this to false means you will see Strings like "\$iw.\$iw.".
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

  import scala.io.AnsiColor.{BOLD, BLUE, GREEN, RED, RESET, YELLOW}

  def color(c: String, s: String) = if (colorOk) BOLD + c + s + RESET else s
  def nameToCode(s: String)       = color(BLUE, s)
  def typeToCode(s: String)       = color(GREEN, s)

  private def label(severity: Severity): String = severity match {
    case internal.Reporter.ERROR   => "error"
    case internal.Reporter.WARNING => "warning"
    case internal.Reporter.INFO    => ""
  }

  protected def clabel(severity: Severity): String = label(severity) match {
    case "" => ""
    case s  => s"$s: "
  }

  def severityColor(severity: Severity): String = severity match {
    case internal.Reporter.ERROR   => RED
    case internal.Reporter.WARNING => YELLOW
    case internal.Reporter.INFO    => RESET
  }

  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit = withoutTruncating {
    val prefix =
      if (colorOk) severityColor(severity) + clabel(severity) + RESET
      else clabel(severity)

    printMessageAt(pos, prefix + msg)
  }

  // indent errors, error message uses the caret to point at the line already on the screen instead of repeating it
  // TODO: can we splice the error into the code the user typed when multiple lines were entered?
  // (should also comment out the error to keep multi-line copy/pastable)
  // TODO: multiple errors are not very intuitive (should the second error for same line repeat the line?)
  // TODO: the console could be empty due to external changes (also, :reset? -- see unfortunate example in jvm/interpreter (plusOne))
  def printMessageAt(posIn: Position, msg: String): Unit = {
    if ((posIn eq null) || (posIn.source eq NoSourceFile)) printMessage(msg)
    else if (posIn.source.file.name == "<console>" && posIn.line == 1) {
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
      def printLineContent() = printMessage(indentation + posIn.lineContent)
      if (isSynthetic) withoutUnwrapping(printLineContent()) else printLineContent()

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
    if (settings.prompt.value) displayPrompt()
  }

  def printMessage(msg: String): Unit =
    if (!totalSilence) printlnAndFlush(msg)
    else if (isTrace) printlnAndFlush("[silent] " + msg)

  def displayPrompt(): Unit =
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
    s"; for details, enable `:setting $name` or `:replay $name`"

  override def finish() = {
    if (hasWarnings) printMessage(s"${StringOps.countElementsAsString(warningCount, label(WARNING))} found")
    if (hasErrors) printMessage(s"${StringOps.countElementsAsString(errorCount, label(ERROR))} found")
  }

}
