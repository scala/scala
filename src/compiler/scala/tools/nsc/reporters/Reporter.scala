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

package scala.tools.nsc
package reporters

import scala.annotation.unused
import scala.collection.mutable
import scala.reflect.internal, util.Position, util.ScalaClassLoader

/** This class exists for sbt compatibility. Global.reporter holds a FilteringReporter.
 *  The only Reporter that is *not* a FilteringReporter is the one created by sbt.
 *  The Global.reporter_= setter wraps that in a delegating [[MakeFilteringForwardingReporter]].
 */
abstract class Reporter extends internal.Reporter {
  // used by sbt
  final def info(pos: Position, msg: String, @unused force: Boolean): Unit = info0(pos, msg, INFO, force = true)

  // allow calling info0 in MakeFilteringForwardingReporter
  private[reporters] final def nonProtectedInfo0(pos: Position, msg: String, severity: Severity): Unit = info0(pos, msg, severity, force = true)

  // overridden by sbt, IDE -- should not be in the reporting interface
  // (IDE receives comments from ScaladocAnalyzer using this hook method)
  // TODO: IDE should override a hook method in the parser instead
  def comment(pos: Position, msg: String): Unit = ()

  // used by sbt (via unit.cancel) to cancel a compile (see hasErrors)
  // TODO: figure out how sbt uses this, come up with a separate interface for controlling the build
  private[this] var _cancelled: Boolean = false
  def cancelled: Boolean = _cancelled
  def cancelled_=(b: Boolean): Unit = _cancelled = b

  override def hasErrors: Boolean = super.hasErrors || cancelled

  override def reset(): Unit = {
    super.reset()
    cancelled = false
  }
}

object Reporter {
  /** The usual way to create the configured reporter.
   *  Errors are reported through `settings.errorFn` and also by throwing an exception.
   */
  def apply(settings: Settings): FilteringReporter = {
    //val loader = ScalaClassLoader(getClass.getClassLoader)  // apply does not make delegate
    val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
    loader.create[FilteringReporter](settings.reporter.value, settings.errorFn)(settings)
  }

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

/** The reporter used in a Global instance.
 *
 *  It filters messages based on
 *    - settings.nowarn
 *    - settings.maxerrs / settings.maxwarns
 *    - positions (only one error at a position, no duplicate messages on a position)
 */
abstract class FilteringReporter extends Reporter {

  def settings: Settings

  // this should be the abstract method all the way up in reflect.internal.Reporter, but sbt compat
  def doReport(pos: Position, msg: String, severity: Severity): Unit

  final protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = doReport(pos, msg, severity)

  private lazy val positions = mutable.Map[Position, Severity]() withDefaultValue INFO
  private lazy val messages  = mutable.Map[Position, List[String]]() withDefaultValue Nil

  private def maxErrors: Int = settings.maxerrs.value
  private def maxWarnings: Int = settings.maxwarns.value

  private def noWarnings: Boolean = settings.nowarn.value

  private var silent = 0

  private def isSilent = silent > 0

  /** Filter all messages while evaluating the block. */
  final def silently[A](body: => A): A = {
    silent += 1
    try body finally silent -= 1
  }

  /**
   * @return 0 to count and display, 1 for no display, 2 for also no count
   */
  override def filter(pos: Position, msg: String, severity: Severity): Int = {
    def maxOk = severity match {
      case internal.Reporter.ERROR   => maxErrors < 0 || errorCount < maxErrors
      case internal.Reporter.WARNING => !noWarnings && (maxWarnings < 0 || warningCount < maxWarnings)
      case _ => true
    }
    if (isSilent || !duplicateOk(pos, severity, msg)) {
      notifySuppressed(pos, msg, severity)
      2 // don't count, don't display
    } else if (!maxOk) 1 // count, but don't display
      else 0
  }

  /** Returns `true` if the message should be reported. Messages are skipped if:
    *   - there was already some error at the position. After an error, no further
    *     messages at that position are issued.
    *   - the same warning/info message was already issued at the same position.
    * Note: two positions are considered identical for logging if they have the same point.
    */
  private def duplicateOk(pos: Position, severity: Severity, msg: String): Boolean = {
    // was a prefix of the msg already reported at this position for purposes of suppressing repetition?
    def matchAt(pos: Position, msg: String): Boolean = messages(pos).exists(msg.startsWith)

    // always report at null / NoPosition
    pos == null || !pos.isDefined || {
      val fpos = pos.focus
      val show = positions(fpos) match {
        case internal.Reporter.ERROR => false               // already error at position
        case s if s.id > severity.id => false               // already message higher than present severity
        case `severity`              => !matchAt(fpos, msg) // already issued this (in)exact message
        case _                       => true                // good to go
      }
      if (show) {
        positions(fpos) = severity
        messages(fpos) ::= Reporter.stripExplanation(msg) // ignore explanatory suffix for suppressing duplicates
      }
      show
    }
  }

  /* Invoked when an error or warning is filtered by position. */
  private def notifySuppressed(pos: Position, msg: String, severity: Severity): Unit = {
    if (settings.prompt) doReport(pos, msg, severity)
    else if (settings.debug) doReport(pos, "[ suppressed ] " + msg, severity)
  }

  override def reset(): Unit = {
    super.reset()
    positions.clear()
    messages.clear()
  }
}
