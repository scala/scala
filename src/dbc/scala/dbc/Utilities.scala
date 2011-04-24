/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc;


/** An object offering transformation methods (views) on various values.
 *  This object's members must be visible in an expression to use value
 *  auto-conversion.
 */
@deprecated(DbcIsDeprecated, "2.9.0") object Utilities {

  implicit def constantToValue (obj: statement.expression.Constant): Value =
    obj.constantValue;

  implicit def valueToConstant (obj: Value): statement.expression.Constant =
    new statement.expression.Constant {
      val constantValue = obj;
    }

}
