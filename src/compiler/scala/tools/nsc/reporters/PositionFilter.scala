/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.internal.util.Position

/** This reporter implements filtering by severity and position.
 */
class PositionFilter(settings: Settings, protected val delegate: InternalReporter) extends InternalReporter with FilteringReporter {

  private val positions = mutable.Map[Position, Severity]() withDefaultValue INFO
  private val messages  = mutable.Map[Position, List[String]]() withDefaultValue Nil

  override def reset(): Unit = {
    positions.clear()
    messages.clear()
    super.reset()
  }

  override protected def filter(pos: Position, msg: String, severity: Severity) =
    severity match {
      case INFO => true
      case WARNING if settings.nowarnings => false
      case _ =>
        val hidden = testAndLog(pos, severity, msg)
        if (hidden && settings.debug) forward(pos, "[ suppressed ] " + msg, severity)
        !hidden
    }

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  private def testAndLog(pos: Position, severity: Severity, msg: String): Boolean =
    pos != null && pos.isDefined && {
      val fpos     = pos.focus
      val suppress = positions(fpos) match {
        case ERROR                    => true  // already error at position
        case highest
          if highest.id > severity.id => true  // already message higher than present severity
        case `severity`               => messages(fpos) contains msg // already issued this exact message
        case _                        => false // good to go
      }

      suppress || {
        positions(fpos) = severity
        messages(fpos) ::= msg
        false
      }
    }
}

/** This reporter filters info messages based on `-verbose`.
 */
class VerboseFilter(settings: Settings, protected val delegate: InternalReporter) extends InternalReporter with FilteringReporter {
  override protected def filter(pos: Position, msg: String, severity: Severity) = settings.verbose || severity != INFO
}
