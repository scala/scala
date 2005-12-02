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
  abstract class Severity(val code : Int);
  object INFO    extends Severity(0);
  object WARNING extends Severity(1);
  object ERROR   extends Severity(2);

  def reset : Unit = {
    errors   = 0;
    warnings = 0;

  }
  def count(severity : Severity): Int = severity match {
    case ERROR   => errors;
    case WARNING => warnings;
    case INFO    => 0;
  };
  def incr(severity : Severity): Unit = severity match {
    case ERROR   => errors   = errors   + 1;
    case WARNING => warnings = warnings + 1;;
    case INFO    => {}
  };
  var errors   : Int = 0;
  var warnings : Int = 0;

  protected def info0(pos : Position, msg : String, severity : Severity, force : Boolean) : Unit;

  def    info(pos : Position, msg : String, force : Boolean) : Unit = info0(pos, msg, INFO   , force);
  def warning(pos : Position, msg : String                 ) : Unit = info0(pos, msg, WARNING, false);
  def   error(pos : Position, msg : String                 ) : Unit = info0(pos, msg,   ERROR, false);
}
