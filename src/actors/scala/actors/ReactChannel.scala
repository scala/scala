/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * @author Philipp Haller
 */
private[actors] class ReactChannel[Msg](receiver: ReplyReactor) extends InputChannel[Msg] {

  private case class SendToReactor(channel: ReactChannel[Msg], msg: Msg)

  /**
   * Sends a message to this <code>ReactChannel</code>.
   *
   * @param  msg the message to be sent
   */
  def !(msg: Msg) {
    receiver ! SendToReactor(this, msg)
  }

  /**
   * Sends a message to this <code>ReactChannel</code>
   * (asynchronous) supplying explicit reply destination.
   *
   * @param  msg     the message to send
   * @param  replyTo the reply destination
   */
  def send(msg: Msg, replyTo: OutputChannel[Any]) {
    receiver.send(SendToReactor(this, msg), replyTo)
  }

  /**
   * Forwards <code>msg</code> to <code>this</code> keeping the
   * last sender as sender instead of <code>self</code>.
   */
  def forward(msg: Msg) {
    receiver forward SendToReactor(this, msg)
  }

  /**
   * Receives a message from this <code>ReactChannel</code>.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  f    a partial function with message patterns and actions
   */
  def react(f: PartialFunction[Msg, Unit]): Nothing = {
    val C = this
    receiver.react {
      case SendToReactor(C, msg) if (f.isDefinedAt(msg.asInstanceOf[Msg])) =>
        f(msg.asInstanceOf[Msg])
    }
  }

  /**
   * Receives a message from this <code>ReactChannel</code> within
   * a certain time span.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   */
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing = {
    val C = this
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.reactWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) =>
        f(msg.asInstanceOf[Msg])
      case TIMEOUT => f(TIMEOUT)
    }
  }

  /**
   * Receives a message from this <code>ReactChannel</code>.
   *
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receive[R](f: PartialFunction[Msg, R]): R = {
    val C = this
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.receive {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) =>
        f(msg.asInstanceOf[Msg])
    }
  }

  /**
   * Receives a message from this <code>ReactChannel</code> within a certain
   * time span.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R = {
    val C = this
    val recvActor = receiver.asInstanceOf[Actor]
    recvActor.receiveWithin(msec) {
      case C ! msg if (f.isDefinedAt(msg.asInstanceOf[Msg])) =>
        f(msg.asInstanceOf[Msg])
      case TIMEOUT => f(TIMEOUT)
    }
  }

  /**
   * Receives the next message from this <code>ReactChannel</code>.
   */
  def ? : Msg = receive {
    case x => x
  }

}
