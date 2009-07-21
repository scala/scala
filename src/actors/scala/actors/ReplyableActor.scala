/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
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
trait ReplyableActor extends ReplyableReactor {
  thiz: AbstractActor with ReplyReactor =>

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   */
  override def !!(msg: Any): Future[Any] = {
    val ftch = new Channel[Any](Actor.self(thiz.scheduler))
    val linkedChannel = new AbstractActor {
      def !(msg: Any) =
        ftch ! msg
      def send(msg: Any, replyTo: OutputChannel[Any]) =
        ftch.send(msg, replyTo)
      def forward(msg: Any) =
        ftch.forward(msg)
      def receiver =
        ftch.receiver
      def linkTo(to: AbstractActor) { /* do nothing */ }
      def unlinkFrom(from: AbstractActor) { /* do nothing */ }
      def exit(from: AbstractActor, reason: AnyRef) {
        ftch.send(Exit(from, reason), thiz)
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
    thiz.linkTo(linkedChannel)
    thiz.send(msg, linkedChannel)
    new Future[Any](ftch) {
      var exitReason: Option[Any] = None
      val handleReply: PartialFunction[Any, Unit] = {
        case Exit(from, reason) =>
          exitReason = Some(reason)
        case any =>
          value = Some(any)
      }

      def apply(): Any =
        if (isSet) {
          if (!value.isEmpty)
            value.get
          else if (!exitReason.isEmpty) {
            val reason = exitReason.get
            if (reason.isInstanceOf[Throwable])
              throw new ExecutionException(reason.asInstanceOf[Throwable])
            else
              throw new ExecutionException(new Exception(reason.toString()))
          }
        } else inputChannel.receive(handleReply andThen {(x: Unit) => apply()})

      def respond(k: Any => Unit): Unit =
 	if (isSet)
          apply()
 	else
          inputChannel.react(handleReply andThen {(x: Unit) => k(apply())})

      def isSet = (value match {
        case None =>
          val handleTimeout: PartialFunction[Any, Boolean] = {
            case TIMEOUT =>
              false
          }
          val whatToDo =
            handleTimeout orElse (handleReply andThen {(x: Unit) => true})
          inputChannel.receiveWithin(0)(whatToDo)
        case Some(_) => true
      }) || !exitReason.isEmpty
    }
  }

}
