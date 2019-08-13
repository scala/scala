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

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.internal.util.Position

/** Filtering by severity and position. */
trait PositionFiltering extends InternalReporter with Filtering {

  /** True to disable warnings. */
  protected def noWarnings: Boolean

  /** Invoked when an error or warning is filtered by position. */
  protected def suppressed(pos: Position, msg: String, severity: Severity): Unit

  private val positions = mutable.Map[Position, Severity]() withDefaultValue INFO
  private val messages  = mutable.Map[Position, List[String]]() withDefaultValue Nil

  override def reset(): Unit = {
    positions.clear()
    messages.clear()
    super.reset()
  }

  override protected def filter(pos: Position, msg: String, severity: Severity) =
    severity match {
      case InternalReporter.INFO => true
      case InternalReporter.WARNING if noWarnings => false
      case _ => !testAndLog(pos, severity, msg) || { suppressed(pos, msg, severity) ; false }
    }

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  private def testAndLog(pos: Position, severity: Severity, msg: String): Boolean =
    pos != null && pos.isDefined && {
      val fpos     = pos.focus
      val suppress = positions(fpos) match {
        case InternalReporter.ERROR   => true  // already error at position
        case highest
          if highest.id > severity.id => true  // already message higher than present severity
        case `severity`               => matchAt(fpos, msg) // already issued this (in)exact message
        case _                        => false // good to go
      }

      suppress || {
        positions(fpos) = severity
        messages(fpos) ::= DisplayReporter.stripExplanation(msg)  // ignore explanatory suffix for suppressing duplicates
        false
      }
    }
  // was a prefix of the msg already reported at this position for purposes of suppressing repetition?
  private def matchAt(pos: Position, msg: String): Boolean = messages(pos).exists(msg.startsWith(_))
}

/** This reporter implements filtering by severity and position.
 */
class PositionFilter(settings: Settings, protected val delegate: InternalReporter) extends InternalReporter with FilteringReporter with PositionFiltering {
  protected def noWarnings = settings.nowarnings
  protected def suppressed(pos: Position, msg: String, severity: Severity): Unit =
    if (settings.prompt) forward(pos, msg, severity)
    else if (settings.debug) forward(pos, "[ suppressed ] " + msg, severity)
}
