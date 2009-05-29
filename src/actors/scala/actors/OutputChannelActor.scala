/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.Queue

trait OutputChannelActor extends OutputChannel[Any] {

  @volatile
  protected var ignoreSender: Boolean = false

  /* The actor's mailbox. */
  protected val mailbox = new MessageQueue

  protected var sendBuffer = new Queue[(Any, OutputChannel[Any])]

  /* A list of the current senders. The head of the list is
   * the sender of the message that was received last.
   */
  protected var senders: List[OutputChannel[Any]] =
    if (ignoreSender) List(null)
    else Nil

  /* If the actor waits in a react, continuation holds the
   * message handler that react was called with.
   */
  protected var continuation: PartialFunction[Any, Unit] = null

  /* Whenever this Actor executes on some thread, waitingFor is
   * guaranteed to be equal to waitingForNone.
   *
   * In other words, whenever waitingFor is not equal to
   * waitingForNone, this Actor is guaranteed not to execute on some
   * thread.
   */
  protected val waitingForNone = (m: Any) => false
  protected var waitingFor: Any => Boolean = waitingForNone

  /**
   * The behavior of an actor is specified by implementing this
   * abstract method.
   */
  def act(): Unit

  protected[actors] def exceptionHandler: PartialFunction[Exception, Unit] =
    Map()

  protected[actors] def scheduler: IScheduler =
    Scheduler

  protected[actors] def mailboxSize: Int =
    mailbox.size

  /**
   * Sends <code>msg</code> to this actor (asynchronous) supplying
   * explicit reply destination.
   *
   * @param  msg      the message to send
   * @param  replyTo  the reply destination
   */
  def send(msg: Any, replyTo: OutputChannel[Any]) {
    val todo = synchronized {
      if (waitingFor ne waitingForNone) {
        val savedWaitingFor = waitingFor
        waitingFor = waitingForNone
        () => scheduler execute {
          var item: Option[(Any, OutputChannel[Any])] =
            synchronized { Some(msg, replyTo) }
          while (!item.isEmpty) {
            if (savedWaitingFor(item.get._1)) {
              resumeReceiver(item.get)
              item = None
            } else {
              mailbox.append(item.get._1, item.get._2)

              item = synchronized {
                if (!sendBuffer.isEmpty)
                  Some(sendBuffer.dequeue())
                else {
                  waitingFor = savedWaitingFor
                  None
                }
              }
            }
          }
        }
      } else {
        sendBuffer.enqueue((msg, replyTo))
        () => { /* do nothing */ }
      }
    }
    todo()
  }

  protected[this] def resumeReceiver(item: (Any, OutputChannel[Any])) {
    if (!ignoreSender)
      senders = List(item._2)
    // assert continuation != null
    (new LightReaction(this, continuation, item._1)).run()
  }

  def !(msg: Any) {
    send(msg, if (ignoreSender) null else Actor.rawSelf(scheduler))
  }

  def forward(msg: Any) {
    send(msg, if (ignoreSender) null else Actor.sender)
  }

  def receiver: Actor = this.asInstanceOf[Actor]

  protected[this] def drainSendBuffer() {
    while (!sendBuffer.isEmpty) {
      val item = sendBuffer.dequeue()
      mailbox.append(item._1, item._2)
    }
  }

  protected[this] def searchMailbox(f: PartialFunction[Any, Unit]) {
    var done = false
    while (!done) {
      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            drainSendBuffer()
            // keep going
          } else {
            waitingFor = f.isDefinedAt
            continuation = f
            done = true
          }
        }
      } else {
        senders = List(qel.session)
        scheduleActor(f, qel.msg)
        done = true
      }
    }
  }

  protected[actors] def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.rawSelf(scheduler) == this, "react on channel belonging to other actor")
    synchronized { drainSendBuffer() }
    searchMailbox(f)
    throw Actor.suspendException
  }

  protected[actors] def sender: OutputChannel[Any] = senders.head

  /**
   * Replies with <code>msg</code> to the sender.
   */
  protected[actors] def reply(msg: Any) {
    sender ! msg
  }

  protected def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) = {
    scheduler execute (new LightReaction(this,
                                         if (f eq null) continuation else f,
                                         msg))
  }

  def start(): OutputChannelActor = {
    scheduler execute {
      scheduler.newActor(OutputChannelActor.this)
      (new LightReaction(OutputChannelActor.this)).run()
    }
    this
  }

  /* This closure is used to implement control-flow operations
   * built on top of `seq`. Note that the only invocation of
   * `kill` is supposed to be inside `Reaction.run`.
   */
  private[actors] var kill: () => Unit =
    () => { exit() }

  private[actors] def seq[a, b](first: => a, next: => b): Unit = {
    val s = Actor.rawSelf(scheduler)
    val killNext = s.kill
    s.kill = () => {
      s.kill = killNext

      // to avoid stack overflow:
      // instead of directly executing `next`,
      // schedule as continuation
      scheduleActor({ case _ => next }, 1)
      throw Actor.suspendException
    }
    first
    throw new KillActorException
  }

  protected[actors] def exit(): Nothing = {
    terminated()
    throw Actor.suspendException
  }

  protected[actors] def terminated() {
    scheduler.terminated(this)
  }

}
