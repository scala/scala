/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package reporters

import java.io.{ BufferedReader, IOException, PrintWriter }
import scala.reflect.internal.util._
import StringOps._

/**
 * This class implements a Reporter that displays messages on a text console.
 */
class ConsoleReporter(val settings: Settings, reader: BufferedReader, err: PrintWriter, out: PrintWriter) extends AbstractReporter {
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) = this(settings, reader, writer, writer)
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  /** maximal number of error messages to be printed */
  final val ERROR_LIMIT = 100

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => null
  }

  private def clabel(severity: Severity): String = {
    val label0 = label(severity)
    if (label0 eq null) "" else label0 + ": "
  }

  private def labelWriter(severity: Severity): PrintWriter =
    if (severity == INFO) out else err

  /** Returns the number of errors issued totally as a string.
   */
  private def getCountString(severity: Severity): String =
    StringOps.countElementsAsString((severity).count, label(severity))

  // internal method where all other printing methods end up;
  // if there is one method to override to intercept everything,
  // this is the one!
  protected def writeImpl(msg: String, writer: PrintWriter): Unit = {
    writer print trimAllTrailingSpace(msg) + "\n"
    writer.flush()
  }

  /** Prints the message. */
  def printMessage(msg: String): Unit = writeImpl(msg, out)

  /** Prints the message with the given position indication. */
  def printMessage(posIn: Position, msg: String): Unit = print(posIn, msg, INFO)

  def print(pos: Position, msg: String, severity: Severity) {
    writeImpl(Position.formatMessage(pos, clabel(severity) + msg, shortname), labelWriter(severity))
  }

  /** Prints the column marker of the given position.
   */
  def printColumnMarker(pos: Position) =
    if (pos.isDefined) { writeImpl(" " * (pos.column - 1) + "^", err) }

  /** Prints the number of errors and warnings if their are non-zero. */
  def printSummary() {
    if (WARNING.count > 0) writeImpl(getCountString(WARNING) + " found", err)
    if (  ERROR.count > 0) writeImpl(getCountString(ERROR  ) + " found", err)
  }

  def display(pos: Position, msg: String, severity: Severity) {
    if (severity != ERROR || severity.count <= ERROR_LIMIT)
      print(pos, msg, severity)
  }

  def displayPrompt(): Unit = {
    out.print("\na)bort, s)tack, r)esume: ")
    out.flush()
    if (reader != null) {
      val response = reader.read().asInstanceOf[Char].toLower
      if (response == 'a' || response == 's') {
        (new Exception).printStackTrace()
        if (response == 'a')
          sys exit 1

        out.print("\n")
        out.flush()
      }
    }
  }

  override def flush() { out.flush(); err.flush() }
}
