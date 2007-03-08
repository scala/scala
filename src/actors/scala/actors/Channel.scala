/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors


/** <p>
 *    This class is used to pattern match on values that were sent
 *    to some channel <code>Chan<sub>n</sub></code> by the current
 *    actor <code>self</code>.
 *  </p>
 *  <p>
 *    The following example demonstrates its usage:
 *  </p><pre>
 *  receive {
 *    case Chan1 ! msg1 => ...
 *    case Chan2 ! msg2 => ...
 *  }
 *  </pre>
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
case class ! [a](ch: InputChannel[a], msg: a)

/**
 * This class provides a means for typed communication among
 * actors. Only the actor creating an instance of a
 * <code>Channel</code> may receive from it.
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
class Channel[Msg] extends InputChannel[Msg] with OutputChannel[Msg] {

  private[actors] var receiver: Actor = synchronized {
    // basically Actor.self, but can be null
    Actor.tl.get.asInstanceOf[Actor]
  }

  private[actors] def this(recv: Actor) = {
    this()
    receiver = recv
  }

  /**
   * Sends a message to this <code>Channel</code>.
   *
   * @param  msg the message to be sent
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

  /**
   * Receives a message from this <code>Channel</code>.
   *
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receive[R](f: PartialFunction[Msg, R]): R = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.receive {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) => f(msg.asInstanceOf[Msg])
    }
  }

  /**
   * Receives a message from this <code>Channel</code> within a certain
   * time span.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.receiveWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  /**
   * Receives a message from this <code>Channel</code>.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  f    a partial function with message patterns and actions
   */
  def react(f: PartialFunction[Msg, Unit]): Nothing = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.react {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) => f(msg.asInstanceOf[Msg])
    }
  }

  /**
   * Receives a message from this <code>Channel</code> within a certain
   * time span.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   */
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.reactWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  /**
   * Sends a message to this <code>Channel</code> and
   * awaits reply.
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  def !?(msg: Msg): Any = {
    val replyChannel = Actor.self.freshReply()
    receiver ! scala.actors.!(this, msg)
    replyChannel.receive {
      case x => x
    }
  }

  /**
   * Sends a message to this <code>Channel</code> and
   * awaits reply within a certain time span.
   *
   * @param  msec the time span before timeout
   * @param  msg  the message to be sent
   * @return      <code>None</code> in case of timeout, otherwise
   *              <code>Some(x)</code> where <code>x</code> is the reply
   */
  def !?(msec: long, msg: Msg): Option[Any] = {
    val replyChannel = Actor.self.freshReply()
    receiver ! scala.actors.!(this, msg)
    replyChannel.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

}
