/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc;

/** An object offering transformation methods (views) on various values.
 *  This object's members must be visible in an expression to use value
 *  auto-conversion.
 */
object Utilities {

  def view (obj: statement.expression.Constant): Value =
    obj.constantValue;

  def view (obj: Value): statement.expression.Constant =
    new statement.expression.Constant {
      val constantValue = obj;
    }

}
