/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement.expression;

case object Default extends Expression {

	/** A SQL-99 compliant string representation of the relation sub-
		* statement. This only has a meaning inside another statement. */
	def sqlInnerString: String = "DEFAULT";

}