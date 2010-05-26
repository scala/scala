/* NSC -- new Scala compiler
 * Copyright 2002-2010 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable.HashMap
import scala.tools.nsc.Settings
import scala.tools.nsc.util.Position

/**
 * This reporter implements filtering.
 */
abstract class AbstractReporter extends Reporter {
  private val positions = new HashMap[Position, Severity]

  override def reset {
    super.reset
    positions.clear
  }

  val settings: Settings

  def display(pos: Position, msg: String, severity: Severity): Unit
  def displayPrompt: Unit

  protected def info0(pos: Position, msg: String, _severity: Severity, force: Boolean) {
    val severity =
      if (settings.Xwarnfatal.value && _severity == WARNING) ERROR
      else _severity

    severity match {
      case INFO =>
        if (force || settings.verbose.value) display(pos, msg, severity)
      case WARNING =>
        val hidden = testAndLog(pos, severity)
        if (!settings.nowarnings.value) {
          if (!hidden || settings.prompt.value) display(pos, msg, severity)
          if (settings.prompt.value) displayPrompt
        }
      case ERROR =>
        val hidden = testAndLog(pos, severity)
        if (!hidden || settings.prompt.value) display(pos, msg, severity)
        if (settings.prompt.value) displayPrompt
    }
  }

  /** Logs a position and returns <code>true</code> if it was already logged.
   *  @note  Two positions are considered identical for logging if they have the same point.
   *
   *  @param pos ...
   *  @return    <code>true</code> if <code>pos</code> was already logged.
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
