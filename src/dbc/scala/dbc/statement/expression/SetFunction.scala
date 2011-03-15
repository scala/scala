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


@deprecated(DbcIsDeprecated) abstract class SetFunction {
  /** A SQL-99 compliant string representation of the set quantifier. */
  def sqlString: String;
}

@deprecated(DbcIsDeprecated) object SetFunction {
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
    def sqlString = sys.error("Binary set function is not supported yet.");
  }
}
