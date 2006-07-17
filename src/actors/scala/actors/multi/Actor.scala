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
trait Actor[T] extends scala.actors.Actor[T] with MailBox {
  def run(): Unit = {}

  def start(): Unit = try { run() }
  catch {
    case d: Done => // do nothing
  }

  def !(msg: T): Unit = send(msg)
}
