/* NSC -- new Scala compiler
 * Copyright 2002-2007 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.reporters

import scala.collection.mutable.HashSet
import nsc.util.Position
import nsc.Settings

/**
 * This reporter implements filtering.
 */
abstract class AbstractReporter extends Reporter {
  private val positions = new HashSet[Position]()

  val settings: Settings

  def display(pos: Position, msg: String, severity: Severity): Unit
  def displayPrompt: Unit

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit =
    severity match {
      case INFO    =>
        if (force || settings.verbose.value) display(pos, msg, severity)
      case WARNING =>
        val hidden = testAndLog(pos)
        if (!settings.nowarnings.value) {
	  if (!hidden || settings.prompt.value) display(pos, msg, severity)
	  if (settings.prompt.value) displayPrompt
        }
      case ERROR =>
        val hidden = testAndLog(pos)
        if (!hidden || settings.prompt.value) display(pos, msg, severity)
        if (settings.prompt.value) displayPrompt
    }

  /** Logs a position and returns <code>true</code> if it was already logged.
   *
   *  @param pos ...
   *  @return    <code>true</code> if <code>pos</code> was already logged.
   */
  private def testAndLog(pos: Position): Boolean = {
    if (pos eq null) return false
    if (pos.column == 0) return false
    if (positions contains pos) return true
    positions += pos
    return false
  }

}
