/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters;
import scala.tools.nsc.util.Position;


/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter {
  var verbose : Boolean = false;
  var  nowarn : Boolean = false;
  var  prompt : Boolean = false;

  def prompt(v : Boolean) : Unit = this.prompt = v;

  def warnings() = warningsx;
  def   errors() =   errorsx;

  protected var warningsx : Int = 0;
  protected var errorsx   : Int = 0;
  def resetCounters() : Unit = {
    warningsx = 0;
      errorsx = 0;
  }

  def    info(pos : Position, msg : String, force : Boolean) : Unit;
  def warning(pos : Position, msg : String                 ) : Unit;
  def   error(pos : Position, msg : String                 ) : Unit;

}
