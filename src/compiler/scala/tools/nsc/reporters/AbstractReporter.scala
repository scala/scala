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

  private val positions = new mutable.HashMap[Position, Severity]

  override def reset() {
    super.reset()
    positions.clear()
  }

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
    import settings.{ nowarnings => noWarnings, _ }
    def info0Prefix(p: String) {
      severity.count += 1
      display(pos, if (p != null) p + msg else msg, severity)
    }
    severity match {
      case INFO =>
        if (verbose || force) info0Prefix(null)
      case WARNING if noWarnings =>
        testAndLog(pos, severity)
      case _ =>
        val hidden = testAndLog(pos, severity)
        if (!hidden || prompt) info0Prefix(null)
        else if (debug) info0Prefix("[ suppressed ] ")
        if (prompt) displayPrompt()
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
