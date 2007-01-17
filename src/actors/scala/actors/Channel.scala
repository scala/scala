/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import Actor._

case object TIMEOUT

class SuspendActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = {
    this
  }
}

case class ![a](ch: Channel[a], msg: a)

/**
 * This class provides a means for typed communication among
 * actors. Only the actor creating an instance of a
 * <code>Channel</code> may receive from it.
 *
 * @version 0.9.2
 * @author Philipp Haller
 */
class Channel[Msg] extends InputChannel[Msg] with OutputChannel[Msg] {

  private[actors] var receiver: Actor = synchronized {
    // basically Actor.self, but can be null
    //Actor.selfs.get(currentThread).asInstanceOf[Actor]
    Actor.tl.get.asInstanceOf[Actor]
  }

  /**
   * Sends <code>msg</code> to this <code>Channel</code>.
   */
  def !(msg: Msg): unit = {
    receiver ! scala.actors.!(this, msg)
  }

  /**
   * Forwards <code>msg</code> to <code>this</code> keeping the
   * last sender as sender instead of <code>self</code>.
   */
  def forward(msg: Msg): unit = {
    receiver forward scala.actors.!(this, msg)
  }

  def receive[R](f: PartialFunction[Any, R]): R = {
    val C = this
    receiver.receive {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
    }
  }

  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    val C = this
    receiver.receiveWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  def react(f: PartialFunction[Any, Unit]): Nothing = {
    val C = this
    receiver.react {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
    }
  }

  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    val C = this
    receiver.reactWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  /**
   * Sends <code>msg</code> to this <code>Channel</code> and
   * awaits reply.
   */
  def !?(msg: Msg): Any = {
    val replyChannel = Actor.self.freshReply()
    receiver ! scala.actors.!(this, msg)
    replyChannel.receive {
      case x => x
    }
  }

  def !?(msec: long, msg: Msg): Option[Any] = {
    val replyChannel = Actor.self.freshReply()
    receiver ! scala.actors.!(this, msg)
    replyChannel.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

}
