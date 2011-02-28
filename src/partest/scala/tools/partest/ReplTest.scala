/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc.interpreter.ILoop

/** A trait for testing repl code.  It drops the first line
 *  of output because the real repl prints a version number.
 */
abstract class ReplTest extends App {
  def code: String
  def eval() = (ILoop run code).lines drop 1
  def show() = eval() foreach println

  show()
}
