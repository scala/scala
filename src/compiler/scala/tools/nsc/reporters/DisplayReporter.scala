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

package scala
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.{Reporter => InternalReporter, ForwardingReporter}
import scala.reflect.internal.util.{Position, StringOps}
import scala.tools.util.SystemExit

import Position.formatMessage
import StringOps.{trimAllTrailingSpace => trimTrailing}

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
    if (hasWarnings) echo(s"${countAs(warningCount, WARNING.toString.toLowerCase)} found")
    if (hasErrors)   echo(s"${countAs(errorCount, ERROR.toString.toLowerCase)} found")
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
trait FilteringReporterOld extends ForwardingReporter with Filtering {
  override def echo(pos: Position, msg: String)    = if (filter(pos, msg, INFO)) delegate.echo(pos, msg)
  override def warning(pos: Position, msg: String) = if (filter(pos, msg, WARNING)) delegate.warning(pos, msg)
  override def error(pos: Position, msg: String)   = if (filter(pos, msg, ERROR)) delegate.error(pos, msg)
}

/** A `Reporter` that counts messages that are passed by the filter. */
trait CountingReporter extends FilteringReporterOld {
  abstract override protected def filter(pos: Position, msg: String, severity: Severity): Boolean =
    super.filter(pos, msg, severity) && { count(severity) ; true }
}

/** Disable a message when super.filter has passed the message but max limit has been reached.
 *  `hasErrors` is implemented as a flag to defer initializing ERROR object.
 */
trait LimitFilter extends FilteringReporterOld {
  def maxerrs: Int
  def maxwarns: Int

  private[this] var warned = false
  private[this] var erred  = false
  override def hasWarnings = warned
  override def hasErrors   = erred

  abstract override protected def filter(pos: Position, msg: String, severity: Severity): Boolean =
    super.filter(pos, msg, severity) && {
    severity match {
      case InternalReporter.ERROR   => erred = true  ; errorCount <= maxerrs
      case InternalReporter.WARNING => warned = true ; warningCount <= maxwarns
      case _       => true
    }}

  override def reset(): Unit = {
    super.reset()
    warned = false
    erred  = false
  }
}
