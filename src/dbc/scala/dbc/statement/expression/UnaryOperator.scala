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


@deprecated(DbcIsDeprecated, "2.9.0") abstract class UnaryOperator extends Expression {

  /** The name of the operator */
  def operator: String;

  /** Whether the operator comes before the operand or not. */
  def operatorIsLeft: Boolean;

  /** The operand applied to the operator. */
  def operand: Expression;

  /** A SQL-99 compliant string representation of the relation sub-
   * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = operatorIsLeft match {
    case true => operator + " " + operand.sqlInnerString;
    case false => operand.sqlInnerString + " " + operator;
  }
}
