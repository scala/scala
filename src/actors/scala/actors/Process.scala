/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

/**
 * @author Philipp Haller
 */
trait Process extends Actor {
  def link(to: Process): Unit
  def linkTo(to: Process): Unit
  def unlink(from: Process): Unit
  def unlinkFrom(from: Process): Unit
  def exit(reason: Symbol): Unit
  def exit(from: Process, reason: Symbol): Unit
}
