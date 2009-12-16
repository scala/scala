/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement


abstract class DerivedColumn {

  /** The value for the column. This value can be of any type but must be
   *  calculated from fields that appear in a relation that takes part
   *  in the query.
   */
  def valueExpression: Expression

  /** A new name for this field. This name must be unique for the query in
   *  which the column takes part.
   */
  def asClause: Option[String]

  /** A SQL-99 compliant string representation of the derived column
   *  sub-statement. This only has a meaning inside a select statement.
   */
  def sqlString: String =
    valueExpression.sqlInnerString +
    (asClause match {
      case None => ""
      case Some(ac) => " AS " + ac
    })

}
