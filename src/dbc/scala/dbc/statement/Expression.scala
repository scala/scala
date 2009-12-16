/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement


/** An expression that calculates some value from fields. */
abstract class Expression extends Relation {

  def fieldTypes: List[DataType] = Nil

  /** A SQL-99 compliant string representation of the expression. */
  def sqlString: String = "SELECT " + sqlInnerString

  /** A SQL-99 compliant string representation of the relation sub-
   *  statement. This only has a meaning inside another statement.
   */
  def sqlInnerString: String

}
