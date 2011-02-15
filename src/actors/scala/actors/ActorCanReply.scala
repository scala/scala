/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import scala.concurrent.SyncVar

/**
 * Provides message send operations that
 * may result in a response from the receiver.
 *
 * @author Philipp Haller
 */
private[actors] trait ActorCanReply extends ReactorCanReply {
  this: AbstractActor with ReplyReactor =>

  override def !?(msg: Any): Any = {
    val replyCh = new Channel[Any](Actor.self(scheduler))
    send(msg, replyCh)
    replyCh.?
  }

  override def !?(msec: Long, msg: Any): Option[Any] = {
    val replyCh = new Channel[Any](Actor.self(scheduler))
    send(msg, replyCh)
    replyCh.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

  override def !![A](msg: Any, handler: PartialFunction[Any, A]): Future[A] = {
    val c = new Channel[A](Actor.self(scheduler))
    val fun = (res: SyncVar[A]) => {
      val ftch = new Channel[A](Actor.self(scheduler))
      send(msg, new OutputChannel[Any] {
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

  override def !!(msg: Any): Future[Any] = {
    val noTransform: PartialFunction[Any, Any] = { case x => x }
    this !! (msg, noTransform)
  }

}
