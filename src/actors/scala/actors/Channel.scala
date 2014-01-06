/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scala.concurrent.SyncVar

/**
 * Used to pattern match on values that were sent to some channel `Chan,,n,,`
 * by the current actor `self`.
 *
 *  @example {{{
 *  receive {
 *    case Chan1 ! msg1 => ...
 *    case Chan2 ! msg2 => ...
 *  }
 *  }}}
 *
 * @author Philipp Haller
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
case class ! [a](ch: Channel[a], msg: a)

/**
 * Provides a means for typed communication among actors. Only the
 * actor creating an instance of a `Channel` may receive from it.
 *
 * @author Philipp Haller
 *
 * @define actor channel
 * @define channel channel
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
class Channel[Msg](val receiver: InternalActor) extends InputChannel[Msg] with OutputChannel[Msg] with CanReply[Msg, Any] {

  type Future[+P] = scala.actors.Future[P]

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
    receiver.receive {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) => f(msg.asInstanceOf[Msg])
    }
  }

  def ? : Msg = receive {
    case x => x
  }

  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R = {
    val C = this.asInstanceOf[Channel[Any]]
    receiver.receiveWithin(msec) {
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
    receiver.reactWithin(msec) {
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

  def !![A](msg: Msg, handler: PartialFunction[Any, A]): Future[A] = {
    val c = new Channel[A](Actor.self(receiver.scheduler))
    val fun = (res: SyncVar[A]) => {
      val ftch = new Channel[A](Actor.self(receiver.scheduler))
      receiver.send(scala.actors.!(this, msg), new OutputChannel[Any] {
        def !(msg: Any) =
          ftch ! handler(msg)
        def send(msg: Any, replyTo: OutputChannel[Any]) =
          ftch.send(handler(msg), replyTo)
        def forward(msg: Any) =
          ftch.forward(handler(msg))
        def receiver =
          ftch.receiver
      })
      ftch.react {
        case any => res.set(any)
      }
    }
    val a = new FutureActor[A](fun, c)
    a.start()
    a
  }

  def !!(msg: Msg): Future[Any] = {
    val noTransform: PartialFunction[Any, Any] = { case x => x }
    this !! (msg, noTransform)
  }

}
