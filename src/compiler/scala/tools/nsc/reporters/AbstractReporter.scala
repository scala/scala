/* NSC -- new Scala compiler
 * Copyright 2002-2011 LAMP/EPFL
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

  private val positions = new mutable.HashMap[Position, Severity]

  override def reset() {
    super.reset
    positions.clear
  }

  private def isVerbose   = settings.verbose.value
  private def noWarnings  = settings.nowarnings.value
  private def isPromptSet = settings.prompt.value

  protected def info0(pos: Position, msg: String, _severity: Severity, force: Boolean) {
    val severity =
      if (settings.fatalWarnings.value && _severity == WARNING) ERROR
      else _severity

    if (severity == INFO) {
      if (isVerbose || force) {
        severity.count += 1
        display(pos, msg, severity)
      }
    }
    else {
      val hidden = testAndLog(pos, severity)
      if (severity == WARNING && noWarnings) ()
      else {
        if (!hidden || isPromptSet) {
          severity.count += 1
          display(pos, msg, severity)
        } else if (settings.debug.value) {
          severity.count += 1
          display(pos, "[ suppressed ] " + msg, severity)
        }

        if (isPromptSet)
          displayPrompt
      }
    }
  }

  /** Logs a position and returns true if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   */
  private def testAndLog(pos: Position, severity: Severity): Boolean =
    pos != null && pos.isDefined && {
      val fpos = pos.focus
      (positions get fpos) match {
        case Some(level) if level >= severity => true
        case _                                => positions += (fpos -> severity) ; false
      }
    }
}
