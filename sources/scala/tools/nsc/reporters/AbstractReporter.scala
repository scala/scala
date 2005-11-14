/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters;
import java.util.HashSet;
import scala.tools.nsc.util.Position;

/**
 * This abstract class implements most aspects of a Reporter, only how
 * things are displayed has to be implemented in subclasses.
 */
abstract class AbstractReporter extends Reporter {
  private val positions = new HashSet();

  def displayInfo   (pos : Position, msg : String) : Unit;
  def displayWarning(pos : Position, msg : String) : Unit;
  def displayError  (pos : Position, msg : String) : Unit;
  def displayPrompt : Unit;

  // XXX: while is pos ignored?
  def    info(pos : Position, msg : String, force : Boolean) : Unit =
    if (force || verbose) displayInfo(pos, msg);

  def warning(pos : Position, msg : String) : Unit = {
    val hidden = testAndLog(pos);
    if (nowarn) return;
    if (!hidden || prompt) displayWarning(pos, msg);
    if (!hidden) warningsx = warningsx + 1;
    if (prompt) displayPrompt;
  }
  def error(pos : Position, msg : String) : Unit = {
    val hidden = testAndLog(pos);
    if (!hidden || prompt) displayError(pos, msg);
    if (!hidden) errorsx = errorsx + 1;
    if (prompt) displayPrompt;
  }

  //########################################################################
  // Private Methods

  /** Logs a position and returns true if it was already logged. */
  private def testAndLog(pos : Position) : Boolean = {
    if (pos == null) return false;
    if (pos.column == 0) return false;
    if (positions.contains(pos)) return true;
    positions.add(pos);
    return false;
  }

  //########################################################################
}
