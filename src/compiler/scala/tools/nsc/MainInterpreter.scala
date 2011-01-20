/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc

/** A command-line wrapper for the interpreter */
object MainInterpreter {
  def main(args: Array[String]) {
    (new InterpreterLoop).main(args)
  }
}
