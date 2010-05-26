/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement
package expression;


abstract class Constant extends Expression {
  /** A SQL-99 compliant string representation of the relation sub-
    * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = constantValue.sqlString;

  /** The value of the constant. */
  def constantValue: Value;
}
