/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The ReplyableReactor trait provides
 * message send operations that may result in a
 * response from the receiver.
 *
 * @author Philipp Haller
 */
trait ReplyableReactor extends Replyable[Any, Any] {
  thiz: ReplyReactor =>

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  def !?(msg: Any): Any = {
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
  def !?(msec: Long, msg: Any): Option[Any] = {
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
   */
  override def !!(msg: Any): Future[Any] = {
    val ftch = new Channel[Any](Actor.rawSelf(thiz.scheduler))
    thiz.send(msg, ftch)
    Futures.fromInputChannel(ftch)
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   * The reply is post-processed using the partial function
   * <code>f</code>. This also allows to recover a more
   * precise type for the reply value.
   */
  override def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A] = {
    val ftch = new Channel[A](Actor.rawSelf(thiz.scheduler))
    thiz.send(msg, new OutputChannel[Any] {
      def !(msg: Any) =
        ftch ! f(msg)
      def send(msg: Any, replyTo: OutputChannel[Any]) =
        ftch.send(f(msg), replyTo)
      def forward(msg: Any) =
        ftch.forward(f(msg))
      def receiver =
        ftch.receiver
    })
    Futures.fromInputChannel(ftch)
  }

}
