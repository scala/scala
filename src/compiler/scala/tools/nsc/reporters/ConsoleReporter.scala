/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.util.{Position, StringOps}
import Position.formatMessage
import StringOps.{countElementsAsString => countAs, trimAllTrailingSpace => trimTrailing}

/** This class implements a Reporter that displays messages on a text console.
 */
class ConsoleReporter(val settings: Settings, reader: BufferedReader, writer: PrintWriter, echoWriter: PrintWriter) extends AbstractReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) =
    this(settings, reader, writer, writer)

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  /** maximal number of error messages to be printed */
  @deprecated("configured by settings.maxerrs", since="2.12.2")
  final val ERROR_LIMIT = 100

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => ""
  }

  protected def clabel(severity: Severity): String = label(severity) match {
    case "" => ""
    case s  => s"$s: "
  }

  /** Prints the message. */
  def printMessage(msg: String): Unit = {
    writer.println(trimTrailing(msg))
    writer.flush()
  }

  /** Prints the message to the echoWriter, which is usually stdout. */
  override def echo(msg: String): Unit = {
    echoWriter.println(trimTrailing(msg))
    echoWriter.flush()
  }

  /** Prints the message with the given position indication. */
  def printMessage(posIn: Position, msg: String): Unit = printMessage(formatMessage(posIn, msg, shortname))

  def print(pos: Position, msg: String, severity: Severity): Unit = printMessage(pos, s"${clabel(severity)}${msg}")

  /** Prints the column marker of the given position. */
  def printColumnMarker(pos: Position): Unit = if (pos.isDefined) printMessage(" " * (pos.column - 1) + "^")

  /** Prints the number of warnings and errors if there are any. */
  def printSummary(): Unit =
    for (k <- List(WARNING, ERROR) if k.count > 0) printMessage(s"${countAs(k.count, label(k))} found")

  def display(pos: Position, msg: String, severity: Severity): Unit = {
    val ok = severity match {
      case ERROR   => ERROR.count   <= settings.maxerrs.value
      case WARNING => WARNING.count <= settings.maxwarns.value
      case _     => true
    }
    if (ok) print(pos, msg, severity)
  }

  def displayPrompt(): Unit = {
    writer.println()
    writer.print("a)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      reader.read match {
        case 'a' | 'A' =>
          new Throwable().printStackTrace(writer)
          System.exit(1)
        case 's' | 'S' =>
          new Throwable().printStackTrace(writer)
          writer.println()
          writer.flush()
        case _ =>
      }
    }
  }

  override def flush() = writer.flush()

  override def finish() = printSummary()
}
