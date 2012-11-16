/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc
package interpreter

/** A command line for the interpreter.
 */
class CommandLine(arguments: List[String], error: String => Unit) extends CompilerCommand(arguments, error) {
  override def cmdName = "scala"
}
