/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * Provides message send operations that
 * may result in a response from the receiver.
 *
 * @author Philipp Haller
 */
private[actors] trait ReactorCanReply extends CanReply[Any, Any] {
  _: InternalReplyReactor =>

  type Future[+P] = scala.actors.Future[P]

  def !?(msg: Any): Any =
    (this !! msg)()

  def !?(msec: Long, msg: Any): Option[Any] = {
    val myself = Actor.rawSelf(this.scheduler)
    val res = new scala.concurrent.SyncVar[Any]
    val out = new OutputChannel[Any] {
      def !(msg: Any) =
        res set msg
      def send(msg: Any, replyTo: OutputChannel[Any]) =
        res set msg
      def forward(msg: Any) =
        res set msg
      def receiver =
        myself.asInstanceOf[Actor]
    }
    this.send(msg, out)
    res.get(msec)
  }

  def !!(msg: Any): Future[Any] =
    this !! (msg, { case x => x })

  def !![A](msg: Any, handler: PartialFunction[Any, A]): Future[A] = {
    val myself = Actor.rawSelf(this.scheduler)
    val ftch = new ReactChannel[A](myself)
    val res = new scala.concurrent.SyncVar[A]

    val out = new OutputChannel[Any] {
      def !(msg: Any) = {
        val msg1 = handler(msg)
        ftch ! msg1
        res set msg1
      }
      def send(msg: Any, replyTo: OutputChannel[Any]) = {
        val msg1 = handler(msg)
        ftch.send(msg1, replyTo)
        res set msg1
      }
      def forward(msg: Any) = {
        val msg1 = handler(msg)
        ftch forward msg1
        res set msg1
      }
      def receiver =
        myself.asInstanceOf[Actor]
    }

    this.send(msg, out)

    new Future[A] {
      def apply() = {
        if (!isSet)
          fvalue = Some(res.get)

        fvalueTyped
      }
      def respond(k: A => Unit): Unit =
        if (isSet) k(fvalueTyped)
        else inputChannel.react {
          case any => fvalue = Some(any); k(fvalueTyped)
        }
      def isSet =
        !fvalue.isEmpty
      def inputChannel = ftch
    }
  }
}
