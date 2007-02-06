/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters

import scala.collection.mutable.HashSet
import nsc.util.Position
import nsc.Settings

/**
 * This reporter implements filtering.
 */
abstract class AbstractReporter extends Reporter {
  private val positions = new HashSet[Position]();

  val settings: Settings

  def display(pos : Position, msg : String, severity : Severity) : Unit
  def displayPrompt : Unit

  protected def info0(pos : Position, msg : String, severity : Severity, force : Boolean) : Unit =
    severity match {
      case INFO    => if (force || settings.verbose.value) display(pos, msg, severity)
      case WARNING => {
        val hidden = testAndLog(pos)
        if (!settings.nowarnings.value) {
	  if (!hidden || settings.prompt.value) display(pos, msg, severity)
	  if (settings.prompt.value) displayPrompt
        }
      }
      case ERROR => {
        val hidden = testAndLog(pos);
        if (!hidden || settings.prompt.value) display(pos, msg, severity)
        if (settings.prompt.value) displayPrompt
      }
    }

  //########################################################################
  // Private Methods

  /** Logs a position and returns true if it was already logged. */
  private def testAndLog(pos : Position) : Boolean = {
    if (pos eq null) return false
    if (pos.column == 0) return false
    if (positions.contains(pos)) return true
    positions += (pos)
    return false
  }

  //########################################################################
}
