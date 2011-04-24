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

/** An insertion of values into a table. */
@deprecated(DbcIsDeprecated, "2.9.0") case class Insert(insertionTarget: String, insertionData: InsertionData)
    extends Status {

  /** A SQL-99 compliant string representation of the select statement. */
  def sqlString: String =
    "INSERT INTO " + insertionTarget + " " + insertionData.sqlString

  /** The name of the table where the data should be added. */
  //def insertionTarget: String

  /** The data that will be added tot he table. */
  //def insertionData: InsertionData

}
