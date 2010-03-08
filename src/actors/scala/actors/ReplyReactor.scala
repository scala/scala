/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.{Timer, TimerTask}

/** <p>
 *    The <code>ReplyReactor</code> trait extends the <code>Reactor</code>
 *    trait with methods to reply to the sender of a message.
 *    Sending a message to a <code>ReplyReactor</code> implicitly
 *    passes a reference to the sender together with the message.
 *  </p>
 *
 *  @author Philipp Haller
 */
trait ReplyReactor extends Reactor[Any] with ReplyableReactor {

  /* A list of the current senders. The head of the list is
   * the sender of the message that was received last.
   */
  @volatile
  private[actors] var senders: List[OutputChannel[Any]] = List()

  /* This option holds a TimerTask when the actor waits in a
   * reactWithin. The TimerTask is cancelled when the actor
   * resumes.
   *
   * guarded by this
   */
  private[actors] var onTimeout: Option[TimerTask] = None

  /**
   * Returns the actor which sent the last received message.
   */
  protected[actors] def sender: OutputChannel[Any] = senders.head

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

  private[actors] override def resumeReceiver(item: (Any, OutputChannel[Any]), handler: PartialFunction[Any, Any], onSameThread: Boolean) {
    synchronized {
      if (!onTimeout.isEmpty) {
        onTimeout.get.cancel()
        onTimeout = None
      }
    }
    senders = List(item._2)
    super.resumeReceiver(item, handler, onSameThread)
  }

  private[actors] override def searchMailbox(startMbox: MQueue[Any],
                                             handler: PartialFunction[Any, Any],
                                             resumeOnSameThread: Boolean) {
    var tmpMbox = startMbox
    var done = false
    while (!done) {
      val qel = tmpMbox.extractFirst((msg: Any, replyTo: OutputChannel[Any]) => {
        senders = List(replyTo)
        handler.isDefinedAt(msg)
      })
      if (tmpMbox ne mailbox)
        tmpMbox.foreach((m, s) => mailbox.append(m, s))
      if (null eq qel) {
        synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            tmpMbox = new MQueue[Any]("Temp")
            drainSendBuffer(tmpMbox)
            // keep going
          } else {
            waitingFor = handler
            // see Reactor.searchMailbox
            throw Actor.suspendException
          }
        }
      } else {
        resumeReceiver((qel.msg, qel.session), handler, resumeOnSameThread)
        done = true
      }
    }
  }

  private[actors] override def makeReaction(fun: () => Unit): Runnable =
    new ReplyReactorTask(this, fun)

  protected[actors] override def react(handler: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.rawSelf(scheduler) == this, "react on channel belonging to other actor")
    super.react(handler)
  }

  /**
   * Receives a message from this actor's mailbox within a certain
   * time span.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  msec     the time span before timeout
   * @param  handler  a partial function with message patterns and actions
   */
  protected[actors] def reactWithin(msec: Long)(handler: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.rawSelf(scheduler) == this, "react on channel belonging to other actor")

    synchronized { drainSendBuffer(mailbox) }

    // first, remove spurious TIMEOUT message from mailbox if any
    mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => m == TIMEOUT)

    while (true) {
      val qel = mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => {
        senders = List(replyTo)
        handler isDefinedAt m
      })
      if (null eq qel) {
        synchronized {
          // in mean time new messages might have arrived
          if (!sendBuffer.isEmpty) {
            drainSendBuffer(mailbox)
            // keep going
          } else if (msec == 0L) {
            // throws Actor.suspendException
            resumeReceiver((TIMEOUT, this), handler, false)
          } else {
            waitingFor = handler
            val thisActor = this
            onTimeout = Some(new TimerTask {
              def run() { thisActor.send(TIMEOUT, thisActor) }
            })
            Actor.timer.schedule(onTimeout.get, msec)
            throw Actor.suspendException
          }
        }
      } else
        resumeReceiver((qel.msg, qel.session), handler, false)
    }
    throw Actor.suspendException
  }

  override def getState: Actor.State.Value = synchronized {
    if (waitingFor ne Reactor.waitingForNone) {
      if (onTimeout.isEmpty)
        Actor.State.Suspended
      else
        Actor.State.TimedSuspended
    } else
      _state
  }

}
