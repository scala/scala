/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Alexander Spoon
 */
// $Id$

package scala.tools.nsc

/** Settings for the interpreter
 *
 * @version 1.0
 * @author Lex Spoon, 2007/3/24
 **/
class InterpreterSettings(repl: Interpreter) {
  /** A list of paths where :load should look */
  var loadPath = List(".")

  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  var maxPrintString = 2400

  def deprecation_=(x: Boolean) = {
    val old = repl.settings.deprecation.value
    repl.settings.deprecation.value = x
    if (!old && x) println("Enabled -deprecation output.")
    else if (old && !x) println("Disabled -deprecation output.")
  }
  def deprecation: Boolean = repl.settings.deprecation.value

  override def toString =
    "InterpreterSettings {\n" +
//    "  loadPath = " + loadPath + "\n" +
    "  maxPrintString = " + maxPrintString + "\n" +
    "}"
}



/* Utilities for the InterpreterSettings class
 *
 * @version 1.0
 * @author Lex Spoon, 2007/5/24
 */
object InterpreterSettings {
  /** Source code for the InterpreterSettings class.  This is
   *  used so that the interpreter is sure to have the code
   *  available.
   */
  val sourceCodeForClass =
"""
package scala.tools.nsc

/** Settings for the interpreter
 *
 * @version 1.0
 * @author Lex Spoon, 2007/3/24
 **/
class InterpreterSettings(repl: Interpreter) {
  /** A list of paths where :load should look */
  var loadPath = List(".")

  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  var maxPrintString = 2400

  def deprecation_=(x: Boolean) = {
    val old = repl.settings.deprecation.value
    repl.settings.deprecation.value = x
    if (!old && x) println("Enabled -deprecation output.")
    else if (old && !x) println("Disabled -deprecation output.")
  }
  def deprecation: Boolean = repl.settings.deprecation.value

  override def toString =
    "InterpreterSettings {\n" +
//    "  loadPath = " + loadPath + "\n" +
    "  maxPrintString = " + maxPrintString + "\n" +
    "}"
}

"""

}
