/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc.value;


abstract class Boolean extends Value {

	val dataType: datatype.Boolean;

	def sqlString = if (nativeValue) "TRUE" else "FALSE";

}

object Boolean {

	def view (obj:value.Boolean): scala.Boolean = obj.nativeValue;

}
