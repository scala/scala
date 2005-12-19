/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement;

import scala.dbc.statement.expression._;

/** Data to be inserted into a table in an <code>Insert</code>. */
case class SetClause (name:String, expr:Expression) {
	val value: Pair[String,Expression] = Pair(name,expr);
	def sqlString: String =
		value._1 + " = " + value._2.sqlInnerString;
}