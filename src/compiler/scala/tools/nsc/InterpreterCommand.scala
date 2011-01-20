/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

/** A command line for the interpreter.
 *
 *  @author  Lex Spoon
 *  @version 1.0
 */
class InterpreterCommand(arguments: List[String], error: String => Unit) extends CompilerCommand(arguments, error) {
  override val cmdName = "scala"
  override lazy val fileEndings = List(".scalaint")
}
