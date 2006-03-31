/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Status.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc.statement;


/** A statement that changes the status of the database. */
abstract class Status extends Statement {

  /** A SQL-99 compliant string representation of the statement. */
  def sqlString: String;

  /** Executes the statement on the given database. */
  def execute (database: scala.dbc.Database): scala.dbc.result.Status[Unit] = {
    database.executeStatement(this);
  }

  def execute (database: scala.dbc.Database, debug: Boolean): scala.dbc.result.Status[Unit] = {
    database.executeStatement(this,debug);
  }

}
