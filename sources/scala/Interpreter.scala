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

  def printValue(value: Any): Unit = {
    if (value == null)
      java.lang.System.out.print("null");
    else
      java.lang.System.out.print(value.toString());
  }

  def showValue(value: Any, tp: String): Unit = {
    printValue(value);
    java.lang.System.out.print(": ");
    java.lang.System.out.println(tp);
  }

  def showDefinition(definition: String): Unit = {
      java.lang.System.out.println(definition);
  }

  def showValueDefinition(definition: String, value: Any): Unit = {
      java.lang.System.out.print(definition);
      java.lang.System.out.print(" = ");
      printValue(value);
      java.lang.System.out.println();
  }

}
