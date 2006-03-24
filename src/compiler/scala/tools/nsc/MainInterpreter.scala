/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Lex Spoon
 */
// $Id$

package scala.tools.nsc

/** A command-line wrapper for the interpreter */
object MainInterpreter {
  def main(args: Array[String]): Unit = {
    (new InterpreterLoop).main(args)
  }
}
