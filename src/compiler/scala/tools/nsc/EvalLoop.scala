/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

trait EvalLoop {

  def prompt: String

  def loop(action: (String) => Unit): Unit = {
    Console.print(prompt)
    val line = Console.readLine
    if ((line ne null) && line.length() > 0) {
      action(line)
      loop(action)
    }
  }

}
