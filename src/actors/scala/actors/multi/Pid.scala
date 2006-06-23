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
[serializable]abstract class Pid {
  def !(msg: MailBox#Message): Unit

  def link(other: Pid): Unit
  def linkTo(other: Pid): Unit // uni-directional
  def unlink(other: Pid): Unit
  def unlinkFrom(other: Pid): Unit // uni-directional
  def exit(reason: Symbol): Unit
  def exit(from: Pid, reason: Symbol): Unit

  def handleExc(destDesc: ExcHandlerDesc, e: Throwable): Unit

  //def become(clos: Actor => Unit): Unit
  //def becomeReceiveLoop(f: PartialFunction[MailBox#Message,unit]): Unit
}
