/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/**
 * The class <code>Pid</code> provides process identifiers
 * to thread-based actors.
 *
 * @author Philipp Haller
 * @version 1.0
 */
class Pid(actor: Actor) {
  private var target = actor

  def !(msg: MailBox#Message) = target send msg

  def spawn(body: Actor => Unit): Pid = {
    val a = new Actor {
      override def run: Unit = body(this)
    }
    a.start
    a.self
  }

  def spawnReceive(cases: PartialFunction[MailBox#Message, Unit]) = {
    val a = new Actor {
      override def run: Unit = receive(cases)
    }
    a.start
    a.self
  }

  override def hashCode() = target.hashCode()

  override def equals(that: Any) =
    this.hashCode() == that.hashCode()

  override def toString() = "Pid(" + target + ")"
}
