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
import scala.tools.util.SystemExit

import Position.formatMessage
import StringOps.{trimAllTrailingSpace => trimTrailing}

/** Facility for outputting messages, with optional user intervention. */
trait PrintReporter extends InternalReporter {

  def settings: Settings
  def reader: BufferedReader
  def writer: PrintWriter
  def echoWriter: PrintWriter

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  private def clabel(severity: Severity): String = severity match {
    case ERROR   => "error: "
    case WARNING => "warning: "
    case _       => ""
  }

  /** Prints the warning or error message. */
  private def printMessage(msg: String): Unit = {
    writer.println(trimTrailing(msg))
    writer.flush()
    if (settings.prompt) displayPrompt()
  }

  /** Prints the message to the echoWriter, which is usually stdout. */
  private def echoMessage(msg: String): Unit = {
    echoWriter.println(trimTrailing(msg))
    echoWriter.flush()
  }

  /** Format a message and emit it. */
  def display(pos: Position, msg: String, severity: Severity): Unit = {
    val text = formatMessage(pos, s"${clabel(severity)}${DisplayReporter.explanation(msg)}", shortname)
    severity match {
      case INFO => echoMessage(text)
      case _    => printMessage(text)
    }
  }

  def displayPrompt(): Unit = {
    writer.println()
    writer.print("a)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      reader.read match {
        case 'a' | 'A' =>
          new Throwable().printStackTrace(writer)
          throw SystemExit(1)
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
    super.flush()
  }

  def close() = {
    writer.close()
    if (writer ne echoWriter) echoWriter.close()
  }
}

/** This class implements a Reporter that displays messages on a text console.
 */
class DisplayReporter(val settings: Settings, val reader: BufferedReader, val writer: PrintWriter, val echoWriter: PrintWriter) extends InternalReporter with PrintReporter {

  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = display(pos, msg, severity)
}

object DisplayReporter {
  def apply(
    settings: Settings      = new Settings,
    reader: BufferedReader  = Console.in,
    writer: PrintWriter     = new PrintWriter(Console.err, true),
    echoWriter: PrintWriter = new PrintWriter(Console.out, true)
  ) = new DisplayReporter(settings, reader, writer, echoWriter)

  /** Take the message with its explanation, if it has one. */
  def explanation(msg: String): String = splitting(msg, explaining = true)

  /** Take the message without its explanation, if it has one. */
  def stripExplanation(msg: String): String = splitting(msg, explaining = false)

  /** Split a message into a prefix and an optional explanation that follows a line starting with `"----"`. */
  private def splitting(msg: String, explaining: Boolean): String =
    if (msg != null && msg.indexOf("\n----") > 0) {
      val (err, exp) = msg.linesIterator.span(!_.startsWith("----"))
      if (explaining) (err ++ exp.drop(1)).mkString("\n") else err.mkString("\n")
    } else {
      msg
    }
}

/** A reporter that filters by position and forwards for display */
class DefaultReporter(settings: Settings, writer: PrintWriter, echo: PrintWriter)
  extends PositionFilter(settings, DisplayReporter(settings, Console.in, writer, echo))
  with CountingReporter
  with SummaryReporter
  with LimitFilter {
  // required constructor for -Xreporter
  def this(settings: Settings) = this(settings, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  private def displayReporter = delegate.asInstanceOf[DisplayReporter]
  def shortname_=(flag: Boolean): Unit = displayReporter.shortname = flag
  def shortname: Boolean = displayReporter.shortname
  def maxerrs = settings.maxerrs.value
  def maxwarns = settings.maxwarns.value
  // closes output writers; reporter will go silent
  def close(): Unit = displayReporter.close()
}
object DefaultReporter {
  def apply(settings: Settings) = new DefaultReporter(settings)
  def apply(settings: Settings, out: PrintWriter) = new DefaultReporter(settings, out, out)
}

/** A `Reporter` that echos a summary in `finish`. */
trait SummaryReporter extends InternalReporter {
  /** Prints the number of warnings and errors if there are any. */
  override def finish(): Unit = {
    import reflect.internal.util.StringOps.{countElementsAsString => countAs}
    if (hasWarnings) echo(s"${countAs(WARNING.count, WARNING.toString.toLowerCase)} found")
    if (hasErrors)   echo(s"${countAs(ERROR.count, ERROR.toString.toLowerCase)} found")
    super.finish()
  }
}

/** Common abstraction for filtering reporter messages. */
trait Filtering { _: InternalReporter =>
  /* True to permit the message. */
  protected def filter(pos: Position, msg: String, severity: Severity): Boolean
}

/** A `ForwardingReporter` that filters events before delegating.
 *
 *  Concrete subclasses should implement just the abstract `filter` method.
 */
trait FilteringReporter extends ForwardingReporter with Filtering {
  override def echo(pos: Position, msg: String)    = if (filter(pos, msg, INFO)) delegate.echo(pos, msg)
  override def warning(pos: Position, msg: String) = if (filter(pos, msg, WARNING)) delegate.warning(pos, msg)
  override def error(pos: Position, msg: String)   = if (filter(pos, msg, ERROR)) delegate.error(pos, msg)
}

/** A `Reporter` that counts messages that are passed by the filter. */
trait CountingReporter extends FilteringReporter {
  abstract override protected def filter(pos: Position, msg: String, severity: Severity): Boolean =
    super.filter(pos, msg, severity) && { severity.count += 1 ; true }
}

/** Disable a message when super.filter has passed the message but max limit has been reached.
 *  `hasErrors` is implemented as a flag to defer initializing ERROR object.
 */
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

  override def reset(): Unit = {
    super.reset()
    warned = false
    erred  = false
  }
}
