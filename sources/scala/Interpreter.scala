/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

module Interpreter {

  def showValue(value: Any): Unit = {
    if (value == null)
      java.lang.System.out.println("null");
    else
      java.lang.System.out.println(value.toString());
  }

  def showDefinition(definition: String): Unit = {
      java.lang.System.out.println(definition);
  }

  def showValueDefinition(definition: String, value: Any): Unit = {
      java.lang.System.out.print(definition);
      java.lang.System.out.print(" = ");
      showValue(value);
  }

}
