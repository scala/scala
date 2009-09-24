/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement
package expression;


case class FunctionCall (
  functionName: String,
  arguments: List[Expression]
) extends Expression {

  /** A SQL-99 compliant string representation of the relation sub-
    * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = {
    functionName + "(" + arguments.mkString("",", ","") + ")"
  }

  /** The name of the function to call. */
  //def functionName: String;

  /** A list of all argument expressions to pass to the function, in order. */
  //def arguments: List[Expression];

}
