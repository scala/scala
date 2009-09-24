/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement;


/** A statement that changes the status of the database. */
case class Transaction [ResultType] (
  transactionBody: (scala.dbc.Database=>ResultType),
  accessMode: Option[AccessMode],
  isolationLevel: Option[IsolationLevel]
) extends Statement {

  /** A SQL-99 compliant string representation of the statement. */
  def sqlStartString: String = (
    "START TRANSACTION" +
    (Pair(accessMode,isolationLevel) match {
      case Pair(None,None) => ""
      case Pair(Some(am),None) => " " + am.sqlString
      case Pair(None,Some(il)) => " " + il.sqlString
      case Pair(Some(am),Some(il)) => " " + am.sqlString + ", " + il.sqlString
    })
  );

  def sqlCommitString: String = {
    "COMMIT"
  }

  def sqlAbortString: String = {
    "ROLLBACK"
  }

  //def transactionBody: (()=>Unit);

  //def accessMode: Option[AccessMode];

  //def isolationLevel: Option[IsolationLevel];

  def execute (database: scala.dbc.Database): scala.dbc.result.Status[ResultType] = {
    database.executeStatement(this);
  }

  def execute (database: scala.dbc.Database, debug: Boolean): scala.dbc.result.Status[ResultType] = {
    database.executeStatement(this,debug);
  }

}
