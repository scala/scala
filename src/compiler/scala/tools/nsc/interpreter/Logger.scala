/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

trait Logger {
  def isInfo: Boolean
  def isDebug: Boolean
  def isTrace: Boolean
  def out: JPrintWriter

  def info(msg: => Any): Unit  = if (isInfo) out println msg
  def debug(msg: => Any): Unit = if (isDebug) out println msg
  def trace(msg: => Any): Unit = if (isTrace) out println msg
}
