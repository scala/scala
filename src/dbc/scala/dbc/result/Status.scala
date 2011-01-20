/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package result;


import scala.dbc.datatype._;

/** An object containing the status of a query */
abstract class Status[ResultType] {

  /** The statement that generated this status result. */
  def statement: scala.dbc.statement.Statement;

  /** The number of elements modified or added by this statement. */
  def touchedCount: Option[Int];

  def result: ResultType;

}
