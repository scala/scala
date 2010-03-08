/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.concurrent.ExecutionException

/**
 * The ReplyableActor trait provides
 * message send operations that may result in a
 * response from the receiver.
 *
 * @author Philipp Haller
 */
private[actors] trait ReplyableActor extends ReplyableReactor {
  thiz: AbstractActor with ReplyReactor =>

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  override def !?(msg: Any): Any = {
    val replyCh = new Channel[Any](Actor.self(thiz.scheduler))
    thiz.send(msg, replyCh)
    replyCh.receive {
      case x => x
    }
  }

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous) within <code>msec</code> milliseconds.
   *
   * @param  msec the time span before timeout
   * @param  msg  the message to be sent
   * @return      <code>None</code> in case of timeout, otherwise
   *              <code>Some(x)</code> where <code>x</code> is the reply
   */
  override def !?(msec: Long, msg: Any): Option[Any] = {
    val replyCh = new Channel[Any](Actor.self(thiz.scheduler))
    thiz.send(msg, replyCh)
    replyCh.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   * The reply is post-processed using the partial function
   * <code>handler</code>. This also allows to recover a more
   * precise type for the reply value.
   */
  override def !![A](msg: Any, handler: PartialFunction[Any, A]): Future[A] = {
    val ftch = new Channel[A](Actor.self(thiz.scheduler))
    thiz.send(msg, new OutputChannel[Any] {
      def !(msg: Any) =
        ftch ! handler(msg)
      def send(msg: Any, replyTo: OutputChannel[Any]) =
        ftch.send(handler(msg), replyTo)
      def forward(msg: Any) =
        ftch.forward(handler(msg))
      def receiver =
        ftch.receiver
    })
    Futures.fromInputChannel(ftch)
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   */
  override def !!(msg: Any): Future[Any] = {
    val ftch = new Channel[Any](Actor.self(thiz.scheduler))
    val linkedChannel = new AbstractActor {
      def !(msg: Any) = {
        ftch ! msg
        thiz unlinkFrom this
      }
      def send(msg: Any, replyTo: OutputChannel[Any]) = {
        ftch.send(msg, replyTo)
        thiz unlinkFrom this
      }
      def forward(msg: Any) = {
        ftch.forward(msg)
        thiz unlinkFrom this
      }
      def receiver =
        ftch.receiver
      def linkTo(to: AbstractActor) { /* do nothing */ }
      def unlinkFrom(from: AbstractActor) { /* do nothing */ }
      def exit(from: AbstractActor, reason: AnyRef) {
        ftch.send(Exit(from, reason), thiz)
        thiz unlinkFrom this
      }
      // should never be invoked; return dummy value
      def !?(msg: Any) = msg
      // should never be invoked; return dummy value
      def !?(msec: Long, msg: Any): Option[Any] = Some(msg)
      // should never be invoked; return dummy value
      override def !!(msg: Any): Future[Any] = {
        val someChan = new Channel[Any](Actor.self(thiz.scheduler))
        Futures.fromInputChannel(someChan)
      }
      // should never be invoked; return dummy value
      override def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A] = {
        val someChan = new Channel[A](Actor.self(thiz.scheduler))
        Futures.fromInputChannel(someChan)
      }
    }
    thiz linkTo linkedChannel
    thiz.send(msg, linkedChannel)
    new Future[Any](ftch) {
      var exitReason: Option[Any] = None
      val handleReply: PartialFunction[Any, Unit] = {
        case Exit(from, reason) =>
          exitReason = Some(reason)
        case any =>
          fvalue = Some(any)
      }

      def apply(): Any =
        if (isSet) {
          if (!fvalue.isEmpty)
            fvalue.get
          else if (!exitReason.isEmpty) {
            val reason = exitReason.get
            if (reason.isInstanceOf[Throwable])
              throw new ExecutionException(reason.asInstanceOf[Throwable])
            else
              throw new ExecutionException(new Exception(reason.toString()))
          }
        } else inputChannel.receive(handleReply andThen { _ => apply() })

      def respond(k: Any => Unit): Unit =
 	if (isSet)
          apply()
 	else
          inputChannel.react(handleReply andThen { _ => k(apply()) })

      def isSet = (fvalue match {
        case None =>
          val handleTimeout: PartialFunction[Any, Boolean] = {
            case TIMEOUT =>
              false
          }
          val whatToDo =
            handleTimeout orElse (handleReply andThen { _ => true })
          inputChannel.receiveWithin(0)(whatToDo)
        case Some(_) => true
      }) || !exitReason.isEmpty
    }
  }

}
