/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
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
 *    <b>case</b> Chan1 ! msg1 => ...
 *    <b>case</b> Chan2 ! msg2 => ...
 *  }
 *  </pre>
 *
 * @author Philipp Haller
 */
case class ! [a](ch: Channel[a], msg: a)

/**
 * This class provides a means for typed communication among
 * actors. Only the actor creating an instance of a
 * <code>Channel</code> may receive from it.
 *
 * @author Philipp Haller
 *
 * @define actor channel
 * @define channel channel
 */
class Channel[Msg](val receiver: Actor) extends InputChannel[Msg] with OutputChannel[Msg] with CanReply[Msg, Any] {

  def this() = this(Actor.self)

  def !(msg: Msg) {
    receiver ! scala.actors.!(this, msg)
  }

  def send(msg: Msg, replyTo: OutputChannel[Any]) {
    receiver.send(scala.actors.!(this, msg), replyTo)
  }

  def forward(msg: Msg) {
    receiver forward scala.actors.!(this, msg)
  }

  def receive[R](f: PartialFunction[Msg, R]): R = {
    val C = this.asInstanceOf[Channel[Any]]
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.receive {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) => f(msg.asInstanceOf[Msg])
    }
  }

  def ? : Msg = receive {
    case x => x
  }

  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R = {
    val C = this.asInstanceOf[Channel[Any]]
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.receiveWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  def react(f: PartialFunction[Msg, Unit]): Nothing = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.react {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) => f(msg.asInstanceOf[Msg])
    }
  }

  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing = {
    val C = this.asInstanceOf[Channel[Any]]
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.reactWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg)) => f(msg)
      case TIMEOUT => f(TIMEOUT)
    }
  }

  def !?(msg: Msg): Any = {
    val replyCh = new Channel[Any](Actor.self(receiver.scheduler))
    receiver.send(scala.actors.!(this, msg), replyCh)
    replyCh.receive {
      case x => x
    }
  }

  def !?(msec: Long, msg: Msg): Option[Any] = {
    val replyCh = new Channel[Any](Actor.self(receiver.scheduler))
    receiver.send(scala.actors.!(this, msg), replyCh)
    replyCh.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

}
