/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement

import scala.dbc.Database
import scala.dbc.result

/** A statement that changes the status of the database. */
@deprecated(DbcIsDeprecated) abstract class Status extends Statement {

  /** A SQL-99 compliant string representation of the statement. */
  def sqlString: String

  /** Executes the statement on the given database. */
  def execute(database: Database): result.Status[Unit] = {
    database.executeStatement(this)
  }

  def execute(database: Database, debug: Boolean): result.Status[Unit] = {
    database.executeStatement(this, debug)
  }

}
