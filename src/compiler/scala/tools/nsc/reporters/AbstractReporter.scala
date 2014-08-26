/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.reflect.internal.util.Position

/**
 * This reporter implements filtering.
 */
abstract class AbstractReporter extends Reporter {
  val settings: Settings
  def display(pos: Position, msg: String, severity: Severity): Unit
  def displayPrompt(): Unit

  private val positions = mutable.Map[Position, Severity]() withDefaultValue INFO
  private val messages  = mutable.Map[Position, List[String]]() withDefaultValue Nil

  override def reset() {
    super.reset()
    positions.clear()
    messages.clear()
  }

  private def isVerbose   = settings.verbose.value
  private def noWarnings  = settings.nowarnings.value
  private def isPromptSet = settings.prompt.value
  private def isDebug     = settings.debug

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
    if (severity == INFO) {
      if (isVerbose || force) {
        severity.count += 1
        display(pos, msg, severity)
      }
    }
    else {
      val hidden = testAndLog(pos, severity, msg)
      if (severity == WARNING && noWarnings) ()
      else {
        if (!hidden || isPromptSet) {
          severity.count += 1
          display(pos, msg, severity)
        }
        else if (isDebug) {
          severity.count += 1
          display(pos, "[ suppressed ] " + msg, severity)
        }

        if (isPromptSet)
          displayPrompt()
      }
    }
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
