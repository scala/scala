/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(arguments: List[String], settings: Settings) extends CompilerCommand(arguments, settings) {
  override def cmdName = "fsc"
}
