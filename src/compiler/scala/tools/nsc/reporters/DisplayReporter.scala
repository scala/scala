/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.{Reporter => InternalReporter, ForwardingReporter}
import scala.reflect.internal.util.{Position, StringOps}

import Position.formatMessage
import StringOps.{countElementsAsString => countAs, trimAllTrailingSpace => trimTrailing}

/** This class implements a Reporter that displays messages on a text console.
 */
class DisplayReporter(settings: Settings, reader: BufferedReader, writer: PrintWriter, echoWriter: PrintWriter) extends InternalReporter {

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case _       => ""
  }

  private def clabel(severity: Severity): String = severity match {
    case ERROR   => "error: "
    case WARNING => "warning: "
    case _       => ""
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

  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = print(pos, msg, severity)

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

  override def flush() = {
    writer.flush()
    if (writer ne echoWriter) echoWriter.flush()
  }

  override def finish() = {
    writer.close()
    if (writer ne echoWriter) echoWriter.close()
  }
}

object DisplayReporter {
  def apply(
    settings: Settings      = new Settings,
    reader: BufferedReader  = Console.in,
    writer: PrintWriter     = new PrintWriter(Console.err, true),
    echoWriter: PrintWriter = new PrintWriter(Console.out, true)
  ) = new DisplayReporter(settings, reader, writer, echoWriter)
}

/** A reporter that filters by position and forwards for display */
class DefaultReporter(settings: Settings, writer: PrintWriter, echo: PrintWriter)
  extends PositionFilter(settings, DisplayReporter(settings, Console.in, writer, echo))
  with CountingReporter
  with LimitFilter
  with SummaryReporter {
  // required constructor for -Xreporter
  def this(settings: Settings) = this(settings, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def shortname_=(flag: Boolean): Unit = delegate.asInstanceOf[DisplayReporter].shortname = flag
  def shortname: Boolean = delegate.asInstanceOf[DisplayReporter].shortname
  def maxerrs = settings.maxerrs.value
  def maxwarns = settings.maxwarns.value
}
object DefaultReporter {
  def apply(settings: Settings) = new DefaultReporter(settings)
  def apply(settings: Settings, out: PrintWriter) = new DefaultReporter(settings, out, out)
}

/** A `ForwardingReporter` that filters events before delegating.
 *
 *  Concrete subclasses should implement just the abstract `filter` method.
 */
trait FilteringReporter extends ForwardingReporter {
  /* True to permit the message. */
  protected def filter(pos: Position, msg: String, severity: Severity): Boolean

  override def echo(pos: Position, msg: String)    = if (filter(pos, msg, INFO)) delegate.echo(pos, msg)
  override def warning(pos: Position, msg: String) = if (filter(pos, msg, WARNING)) delegate.warning(pos, msg)
  override def error(pos: Position, msg: String)   = if (filter(pos, msg, ERROR)) delegate.error(pos, msg)
}

trait CountingReporter extends FilteringReporter {
  abstract override protected def filter(pos: Position, msg: String, severity: Severity): Boolean =
    super.filter(pos, msg, severity) && { severity.count += 1 ; true }
}

trait LimitFilter extends FilteringReporter {
  def maxerrs: Int
  def maxwarns: Int

  private[this] var warned = false
  private[this] var erred  = false
  override def hasWarnings = warned
  override def hasErrors   = erred

  abstract override protected def filter(pos: Position, msg: String, severity: Severity): Boolean =
    super.filter(pos, msg, severity) && {
    severity match {
      case ERROR   => erred = true  ; ERROR.count <= maxerrs
      case WARNING => warned = true ; WARNING.count <= maxwarns
      case _       => true
    }}
}

/** A `Reporter` that echos a summary in `finish`.
 */
trait SummaryReporter extends InternalReporter { _: CountingReporter =>
  /** Prints the number of warnings and errors if there are any. */
  override def finish(): Unit = {
    import reflect.internal.util.StringOps.{countElementsAsString => countAs}
    if (hasWarnings) echo(s"${countAs(WARNING.count, WARNING.toString.toLowerCase)} found")
    if (hasErrors)   echo(s"${countAs(ERROR.count, ERROR.toString.toLowerCase)} found")
    super.finish()
  }
}

trait TracingReporter extends ForwardingReporter {
  private def debug(pos: Position, msg: String, sev: Severity) = println(s"[$sev] $pos $msg")
  override def echo(pos: Position, msg: String)    = { debug(pos, msg, INFO); delegate.echo(pos, msg) }
  override def warning(pos: Position, msg: String) = { debug(pos, msg, WARNING); delegate.warning(pos, msg) }
  override def error(pos: Position, msg: String)   = { debug(pos, msg, ERROR); delegate.error(pos, msg) }
}
