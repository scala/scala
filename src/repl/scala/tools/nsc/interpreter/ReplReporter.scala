/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import reporters._
import IMain._
import scala.reflect.internal.util.{NoPosition, NoSourceFile, Position}

/** Like ReplGlobal, a layer for ensuring extra functionality.
 */
class  ReplReporter(intp: IMain) extends ConsoleReporter(intp.settings, Console.in, new ReplStrippingWriter(intp)) {
  def printUntruncatedMessage(msg: String) = withoutTruncating(printMessage(msg))

  /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = !intp.settings.verbose
  def truncationOK = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  override def warning(pos: Position, msg: String): Unit = withoutTruncating(super.warning(pos, msg))
  override def error(pos: Position, msg: String): Unit   = withoutTruncating(super.error(pos, msg))

  import scala.io.AnsiColor.{ RED, YELLOW, RESET }

  def severityColor(severity: Severity): String = severity match {
    case ERROR   => RED
    case WARNING => YELLOW
    case INFO    => RESET
  }

  override def print(pos: Position, msg: String, severity: Severity) {
    val prefix = (
      if (replProps.colorOk)
        severityColor(severity) + clabel(severity) + RESET
      else
        clabel(severity)
    )
    printMessage(pos, prefix + msg)
  }

  private lazy val indentDepth = replProps.promptText.lines.toList.last.length
  private lazy val indentation = " " * indentDepth
  private def indented(str: String) = str.lines.mkString(indentation, "\n" + indentation, "")

  // indent errors, error message uses the caret to point at the line already on the screen instead of repeating it
  // TODO: can we splice the error into the code the user typed when multiple lines were entered?
  // (should also comment out the error to keep multi-line copy/pastable)
  // TODO: multiple errors are not very intuitive (should the second error for same line repeat the line?)
  // TODO: the console could be empty due to external changes (also, :reset? -- see unfortunate example in jvm/interpeter (plusOne))
  override def printMessage(posIn: Position, msg: String): Unit = {
    if ((posIn eq null) || (posIn.source eq NoSourceFile)) printMessage(msg)
    else {
      val currentRequest = intp.currentRequest
      val consoleLinePrefix = "On line "
      val locationPrefix =
        posIn.source.file.name match { case  "<console>" => consoleLinePrefix case n => s"$n:" }

      // If there's only one line of input, and it's already printed on the console (as indicated by the position's source file name),
      // reuse that line in our error output, and suppress the line number (since we know it's `1`)
      if (locationPrefix == consoleLinePrefix && currentRequest.originalLine.indexOf('\n') == -1) {
        printMessage(indentation + posIn.lineCaret)
        printMessage(indented(msg))
      }
      else {
        val preambleLineDelta = currentRequest.preambleEndPos.line
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
    // Avoiding deadlock if the compiler starts logging before
    // the lazy val is complete.
    if (intp.isInitializeComplete) {
      if (intp.totalSilence) {
        if (isReplTrace)
          super.printMessage("[silent] " + msg)
      }
      else super.printMessage(msg)
    }
    else Console.println("[init] " + msg)
  }

  override def displayPrompt(): Unit =
    if (!intp.totalSilence) super.displayPrompt()

  override def rerunWithDetails(setting: reflect.internal.settings.MutableSettings#Setting, name: String) =
    s"; for details, enable `:setting $name' or `:replay $name'"

}
