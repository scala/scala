/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement
package expression;


@deprecated(DbcIsDeprecated, "2.9.0") abstract class BinaryOperator extends Expression {

  /** The name of the operator. */
  def operator: String;

  /** The expression applied on the left of the operator. */
  def leftOperand: Expression;

  /** The expression applied on the right of the operator. */
  def rightOperand: Expression;

  /** A SQL-99 compliant string representation of the relation sub-
   * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = {
    leftOperand.sqlInnerString + " " + operator + " " + rightOperand.sqlInnerString
  }

}
