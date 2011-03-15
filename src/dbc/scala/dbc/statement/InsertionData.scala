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
@deprecated(DbcIsDeprecated) abstract class InsertionData {
  def sqlString: String
}

@deprecated(DbcIsDeprecated) object InsertionData {
  /** Insertion of data resulting from a query on the database. */
  @deprecated(DbcIsDeprecated) case class Subquery(query: Relation) extends InsertionData {
    def sqlString = query.sqlString
  }
  /** Insertion of data as explicitly defined values. */
  @deprecated(DbcIsDeprecated) case class Constructor(
    columnNames: Option[List[String]],
    columnValues: List[Expression]
  ) extends InsertionData {
    def sqlString =
      (columnNames match {
        case None => ""
        case Some(cn) => cn.mkString(" (",", ",")")
      }) +
      " VALUES" +
      columnValues.map(e => e.sqlInnerString).mkString(" (",", ",")")
  }
}
