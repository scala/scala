/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Status.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


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
