/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package value;


abstract class Boolean extends Value {

  val dataType: datatype.Boolean;

  def sqlString = if (nativeValue) "TRUE" else "FALSE";

}

object Boolean {

  implicit def booleanToBoolean (obj:value.Boolean): scala.Boolean = obj.nativeValue;

}
