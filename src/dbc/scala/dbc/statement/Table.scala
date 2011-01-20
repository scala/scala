/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


/** A reference to a table in the database.
  * @author Gilles Dubochet
  * @version 1.0 */
abstract class Table extends Relation {

  /** The name of the table in the database. */
  def tableName: String

  /** The name that the table will be called in the enclosing statement. */
  def tableRename: Option[String]

  /** A SQL-99 compliant string representation of the relation statement. */
  def sqlString: String = "SELECT * FROM " + tableName

  /** A SQL-99 compliant string representation of the relation sub-
    * statement. This only has a meaning inside a query. */
  def sqlInnerString: String =
    tableName +
    (tableRename match {
      case None => ""
      case Some(rename) => " AS " + rename
    })

}
