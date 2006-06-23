/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.multi

/**
 * @author Philipp Haller
 */
trait AbstractPid {
  def !(msg: MailBox#Message): Unit
  def become(clos: Actor => Unit): Unit
  def becomeReceiveLoop(f: PartialFunction[MailBox#Message,Unit]): Unit
}
