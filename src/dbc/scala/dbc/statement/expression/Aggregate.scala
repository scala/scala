/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement
package expression


@deprecated(DbcIsDeprecated) abstract class Aggregate extends Expression {

  def aggregateName: String;

  def setFunction: SetFunction;

  def filterClause: Option[Expression];

  /** A SQL-99 compliant string representation of the relation sub-
   * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = (
    aggregateName +
    "(" + setFunction.sqlString + ")" +
    (filterClause match {
      case None => ""
      case Some(fc) => " FILTER (WHERE " + fc.sqlString + ")"
    })
  )

}
