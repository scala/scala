/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


import scala.dbc.statement.expression._

/** Data to be inserted into a table in an <code>Insert</code>. */
@deprecated(DbcIsDeprecated, "2.9.0") case class SetClause(name: String, expr: Expression) {
  val value: Pair[String,Expression] = (name, expr)
  def sqlString: String = value._1 + " = " + value._2.sqlInnerString
}
