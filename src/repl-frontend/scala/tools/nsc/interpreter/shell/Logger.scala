/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc.interpreter.shell

import java.io.PrintWriter

trait Logger {
  def isInfo: Boolean
  def isDebug: Boolean
  def isTrace: Boolean
  def out: PrintWriter
}
