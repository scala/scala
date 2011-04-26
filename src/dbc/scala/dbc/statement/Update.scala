/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement;


import scala.dbc.statement.expression._;

/** An update of the state of a table. */
@deprecated(DbcIsDeprecated, "2.9.0") case class Update (
  updateTarget: String,
  setClauses: List[SetClause],
  whereClause: Option[Expression]
) extends Status {


  /** A SQL-99 compliant string representation of the select statement. */
  def sqlString: String = (
    "UPDATE " +
    updateTarget +
    " SET " + setClauses.map(sc=>sc.sqlString).mkString("",", ","") +
    (whereClause match {
      case None => ""
      case Some(expr) => " WHERE " + expr.sqlString
    })
  );

  /** The name of the table that should be updated. */
  //def updateTarget: String;

  /** The data that will be added tot he table. */
  //def setClauses: List[SetClause];

  /** Defines condition that must be true in the tuples that will be updated.
    * This value expression must return a boolean or boolean-compatible
    * value. */
  //def whereClause: Option[scala.dbc.statement.expression.Expression];

}
