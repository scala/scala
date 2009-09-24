/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc


import java.sql._

/** A link to a database. The <code>Database</code> abstract class must
 *  be specialised for every different DBMS.
 *
 *  @author  Gilles Dubochet
 */
case class Database(dbms: Vendor) {

  class Closed extends Exception {}

  /** A lock used for operations that need to be atomic for this database
   *  instance. */
  private val lock: scala.concurrent.Lock = new scala.concurrent.Lock()

  /** The vendor of the DBMS that contains this database. */
  private val vendor: Vendor = dbms

  /** The Database connections available to use. */
  private var availableConnections: List[Connection] = Nil

  /** The connections that are currently in use. */
  private var usedConnections: List[Connection] = Nil

  /** Whether the database no longer accepts new connections. */
  private var closing: Boolean = false;

  /** Retrieves a connection from the available connection pool or creates
   *  a new one.
   *
   *  @return A connection that can be used to access the database.
   */
  private def getConnection: Connection = {
    if (closing) {
      throw new Closed;
    } else {
      availableConnections match {
        case Nil => {
          lock.acquire;
          val connection = vendor.getConnection;
          usedConnections = connection :: usedConnections;
          lock.release;
          connection
        }
        case connection :: cs => {
          lock.acquire;
          availableConnections = cs;
          usedConnections = connection :: usedConnections;
          lock.release;
          connection;
        }
      }
    }
  }

  /** Closes a connection to this database. A closed connection might
   *  also return to the available connection pool if the latter is depleted.
   *
   *  @param connection The connection that should be closed.
   */
  private def closeConnection(connection: Connection): Unit = {
    if (closing) {
      connection.close()
    } else {
      lock.acquire
      usedConnections = usedConnections.filterNot(e => (e.equals(connection)));
      if (availableConnections.length < vendor.retainedConnections)
        availableConnections = connection :: availableConnections
      else
        connection.close()
      lock.release
    }
  }

  /** ..
   */
  def close {
    closing = true
    for (conn <- availableConnections) conn.close()
  }

  /** Executes a statement that returns a relation on this database.
   *
   *  @param  relationStatement The statement to execute.
   *  @return The relation returned by the database for this statement.
   */
  def executeStatement(relationStatement: statement.Relation): result.Relation =
    executeStatement(relationStatement, false);

  /** Executes a statement that returns a relation on this database.
   *
   *  @param relationStatement The statement to execute.
   *  @param debug Whether debugging information should be printed on the console.
   *  @return The relation returned by the database for this statement.
   */
  def executeStatement(relationStatement: statement.Relation,
                       debug: Boolean): result.Relation =
    new scala.dbc.result.Relation {
      val statement = relationStatement
      if (debug) Console.println("## " + statement.sqlString)
      private val connection = getConnection
      val sqlResult = connection.createStatement().executeQuery(statement.sqlString)
      closeConnection(connection)
      statement.typeCheck(this)
    }

  /** Executes a statement that updates the state of the database.
    * @param statusStatement The statement to execute.
    * @return The status of the database after the statement has been executed. */
  def executeStatement(statusStatement: statement.Status): result.Status[Unit] =
    executeStatement(statusStatement, false);

  /** Executes a statement that updates the state of the database.
   *
   *  @param  statusStatement The statement to execute.
   *  @param  debug Whether debugging information should be printed on the console.
   *  @return The status of the database after the statement has been executed.
   */
  def executeStatement(statusStatement: statement.Status,
                       debug: Boolean): result.Status[Unit] =
    new scala.dbc.result.Status[Unit] {
      val statement = statusStatement;
      if (debug) Console.println("## " + statement.sqlString);
      def result = ();
      private val connection = getConnection;
      val jdbcStatement: java.sql.Statement = connection.createStatement();
      jdbcStatement.execute(statement.sqlString);
      val touchedCount = Some(jdbcStatement.getUpdateCount());
      closeConnection(connection);
    }

  /** Executes a list of statements or other operations inside a transaction.
   *  Only statements are protected in a transaction, other Scala code is not.
   *
   *  @param  transactionStatement The transaction to execute as a closure.
   *  @return The status of the database after the transaction has been executed.
   */
  def executeStatement[ResultType](transactionStatement: statement.Transaction[ResultType]): result.Status[ResultType] =
    executeStatement(transactionStatement, false);

  /** Executes a list of statements or other operations inside a transaction.
   *  Only statements are protected in a transaction, other Scala code is not.
   *
   *  @param  transactionStatement The transaction to execute as a closure.
   *  @param  debug Whether debugging information should be printed on the console.
   *  @return The status of the database after the transaction has been executed.
   */
  def executeStatement[ResultType](transactionStatement: statement.Transaction[ResultType], debug: Boolean): result.Status[ResultType] = {
    new scala.dbc.result.Status[ResultType] {
      val touchedCount = None
      val statement = transactionStatement
      private val connection = getConnection
      connection.setAutoCommit(false)
      val jdbcStatement: java.sql.Statement = connection.createStatement();
      if (debug) Console.println("## " + transactionStatement.sqlStartString);
      jdbcStatement.execute(transactionStatement.sqlStartString);
      val result: ResultType = try {
        val buffer = transactionStatement.transactionBody(Database.this);
        if (debug) Console.println("## " + transactionStatement.sqlCommitString);
        jdbcStatement.execute(transactionStatement.sqlCommitString);
        buffer
      } catch {
        case e: Throwable => {
          if (debug) Console.println("## " + transactionStatement.sqlAbortString);
          jdbcStatement.execute(transactionStatement.sqlAbortString);
          throw e
        }
      }
      connection.setAutoCommit(true)
      closeConnection(connection)
    }
  }

}
