/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package value;


@deprecated(DbcIsDeprecated, "2.9.0") abstract class Boolean extends Value {

  val dataType: datatype.Boolean;

  def sqlString = if (nativeValue) "TRUE" else "FALSE";

}

@deprecated(DbcIsDeprecated, "2.9.0") object Boolean {

  implicit def booleanToBoolean (obj:value.Boolean): scala.Boolean = obj.nativeValue;

}
