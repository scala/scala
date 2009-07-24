/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:SetFunction.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package statement
package expression;


abstract class SetFunction {
  /** A SQL-99 compliant string representation of the set quantifier. */
  def sqlString: String;
}

object SetFunction {
  abstract class Asterisk extends SetFunction {
    def sqlString = "(*)";
  }
  abstract class General extends SetFunction {
    def setQuantifier: Option[SetQuantifier];
    def valueExpression: Expression;
    def sqlString = (
      "(" +
      (setQuantifier match {
        case None => ""
        case Some(sq) => sq.sqlString + " "
      }) +
      valueExpression.sqlString + ")"
    );
  }
  abstract class Binary extends SetFunction {
    def sqlString = error("Binary set function is not supported yet.");
  }
}
