/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package reporters

import java.io.{ BufferedReader, PrintWriter }
import scala.reflect.internal.util._
import StringOps._

/** This class implements a Reporter that displays messages on a text console.
 */
class ConsoleReporter(val settings: Settings, reader: BufferedReader, writer: PrintWriter) extends AbstractReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true))

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  /** maximal number of error messages to be printed */
  final val ERROR_LIMIT = 100

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => null
  }

  protected def clabel(severity: Severity): String = {
    val label0 = label(severity)
    if (label0 eq null) "" else label0 + ": "
  }

  /** Returns the number of errors issued totally as a string.
   */
  private def getCountString(severity: Severity): String =
    StringOps.countElementsAsString((severity).count, label(severity))

  /** Prints the message. */
  def printMessage(msg: String) {
    writer print trimAllTrailingSpace(msg) + "\n"
    writer.flush()
  }

  /** Prints the message with the given position indication. */
  def printMessage(posIn: Position, msg: String) {
    printMessage(Position.formatMessage(posIn, msg, shortname))
  }
  def print(pos: Position, msg: String, severity: Severity) {
    printMessage(pos, clabel(severity) + msg)
  }

  /** Prints the column marker of the given position.
   */
  def printColumnMarker(pos: Position) =
    if (pos.isDefined) { printMessage(" " * (pos.column - 1) + "^") }

  /** Prints the number of errors and warnings if their are non-zero. */
  def printSummary() {
    if (WARNING.count > 0) printMessage(getCountString(WARNING) + " found")
    if (  ERROR.count > 0) printMessage(getCountString(ERROR  ) + " found")
  }

  def display(pos: Position, msg: String, severity: Severity) {
    if (severity != ERROR || severity.count <= ERROR_LIMIT)
      print(pos, msg, severity)
  }

  def displayPrompt(): Unit = {
    writer.print("\na)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        (new Exception).printStackTrace()
        if (response == 'a')
          sys exit 1

        writer.print("\n")
        writer.flush()
      }
    }
  }

  override def flush() = writer.flush()

  override def finish() = printSummary()
}
