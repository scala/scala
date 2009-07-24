/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Boolean.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package value;


abstract class Boolean extends Value {

  val dataType: datatype.Boolean;

  def sqlString = if (nativeValue) "TRUE" else "FALSE";

}

object Boolean {

  implicit def booleanToBoolean (obj:value.Boolean): scala.Boolean = obj.nativeValue;

}
