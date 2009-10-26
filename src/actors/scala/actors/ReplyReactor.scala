/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/** <p>
 *    The <code>ReplyReactor</code> trait extends the <code>Reactor</code>
 *    trait with methods to reply to the sender of a message.
 *    Sending a message to a <code>ReplyReactor</code> implicitly
 *    passes a reference to the sender together with the message.
 *  </p>
 *
 *  @author Philipp Haller
 */
trait ReplyReactor extends Reactor with ReplyableReactor {

  /* A list of the current senders. The head of the list is
   * the sender of the message that was received last.
   */
  @volatile
  private[actors] var senders: List[OutputChannel[Any]] =
    Nil

  protected[actors] def sender: OutputChannel[Any] =
    senders.head

  /**
   * Replies with <code>msg</code> to the sender.
   */
  protected[actors] def reply(msg: Any) {
    sender ! msg
  }

  /**
   * Sends <code>msg</code> to this actor (asynchronous).
   */
  override def !(msg: Any) {
    send(msg, Actor.rawSelf(scheduler))
  }

  /**
   * Forwards <code>msg</code> to this actor (asynchronous).
   */
  override def forward(msg: Any) {
    send(msg, Actor.sender)
  }

  private[actors] override def resumeReceiver(item: (Any, OutputChannel[Any]), onSameThread: Boolean) {
    senders = List(item._2)
    // assert continuation != null
    if (onSameThread)
      continuation(item._1)
    else
      scheduleActor(continuation, item._1)
  }

  // assume continuation != null
  private[actors] override def searchMailbox(startMbox: MessageQueue,
                                             handlesMessage: Any => Boolean,
                                             resumeOnSameThread: Boolean) {
    var tmpMbox = startMbox
    var done = false
    while (!done) {
      val qel = tmpMbox.extractFirst((msg: Any, replyTo: OutputChannel[Any]) => {
        senders = List(replyTo)
        handlesMessage(msg)
      })
      if (tmpMbox ne mailbox)
        tmpMbox.foreach((m, s) => mailbox.append(m, s))
      if (null eq qel) {
        synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            tmpMbox = new MessageQueue("Temp")
            drainSendBuffer(tmpMbox)
            // keep going
          } else {
            waitingFor = handlesMessage
            done = true
          }
        }
      } else {
        resumeReceiver((qel.msg, qel.session), resumeOnSameThread)
        done = true
      }
    }
  }

}
