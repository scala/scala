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
  def !?(msg: Any): Any =
    (this !! msg)()

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
    val myself = Actor.rawSelf(thiz.scheduler)
    val res = new scala.concurrent.SyncVar[Any]
    val out = new OutputChannel[Any] {
      def !(msg: Any) =
        res set msg
      def send(msg: Any, replyTo: OutputChannel[Any]) =
        res set msg
      def forward(msg: Any) =
        res set msg
      def receiver =
        myself
    }
    thiz.send(msg, out)
    res.get(msec)
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   */
  override def !!(msg: Any): Future[Any] =
    this !! (msg, { case x => x })

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   * The reply is post-processed using the partial function
   * <code>f</code>. This also allows to recover a more
   * precise type for the reply value.
   */
  override def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A] = {
    val myself = Actor.rawSelf(thiz.scheduler)
    val ftch = new Channel[A](myself)
    val res = new scala.concurrent.SyncVar[A]

    val out = new OutputChannel[Any] {
      def !(msg: Any) = {
        ftch ! f(msg)
        res set f(msg)
      }
      def send(msg: Any, replyTo: OutputChannel[Any]) = {
        ftch.send(f(msg), replyTo)
        res set f(msg)
      }
      def forward(msg: Any) = {
        ftch forward f(msg)
        res set f(msg)
      }
      def receiver =
        myself
    }

    thiz.send(msg, out)

    new Future[A](ftch) {
      def apply() =
        if (isSet) value.get.asInstanceOf[A]
        else {
          value = Some(res.get)
          value.get.asInstanceOf[A]
        }
      def respond(k: A => Unit): Unit =
        if (isSet) k(value.get.asInstanceOf[A])
        else inputChannel.react {
 	  case any => value = Some(any); k(value.get.asInstanceOf[A])
        }
      def isSet =
        !value.isEmpty
    }
  }

}
