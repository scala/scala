/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

/** a command line for the interpreter */
class InterpreterCommand(arguments: List[String], error: String => unit)
extends CompilerCommand(arguments, new Settings(error), error, false) {
  override val cmdName = "scala"
  override val fileEnding = ".scalaint"
}
