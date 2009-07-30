/* NSC -- new Scala compiler
 * Copyright 2002-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

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

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
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
  private def testAndLog(pos: Position, severity: Severity): Boolean = {
    if (pos eq null) return false
    if (pos.offset.isEmpty) return false
    val fpos = pos.focus
    if ((positions contains fpos) && positions(fpos) >= severity) return true
    positions += (fpos -> severity)
    false
  }
}
