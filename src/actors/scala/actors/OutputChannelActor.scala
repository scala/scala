/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

trait OutputChannelActor extends OutputChannel[Any] {

  protected var ignoreSender: Boolean = false

  /* The actor's mailbox. */
  protected val mailbox = new MessageQueue

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

  protected[actors] def exceptionHandler: PartialFunction[Exception, Unit] = Map()

  protected[actors] def scheduler: IScheduler =
    Scheduler

  def mailboxSize: Int = synchronized {
    mailbox.size
  }

  def send(msg: Any, replyTo: OutputChannel[Any]) = synchronized {
    if (waitingFor(msg)) {
      waitingFor = waitingForNone

      if (!ignoreSender)
        senders = List(replyTo)

      // assert continuation != null
      scheduler.execute(new LightReaction(this, continuation, msg))
    } else {
      mailbox.append(msg, replyTo)
    }
  }

  def !(msg: Any) {
    send(msg, if (ignoreSender) null else Actor.rawSelf(scheduler))
  }

  def forward(msg: Any) {
    send(msg, if (ignoreSender) null else Actor.sender)
  }

  def receiver: Actor = this.asInstanceOf[Actor]

  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.rawSelf(scheduler) == this, "react on channel belonging to other actor")
    this.synchronized {
      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        waitingFor = f.isDefinedAt
        continuation = f
      } else {
        if (!ignoreSender)
          senders = List(qel.session)
        scheduleActor(f, qel.msg)
      }
      throw new SuspendActorException
    }
  }

  def sender: OutputChannel[Any] = senders.head

  /**
   * Replies with <code>msg</code> to the sender.
   */
  def reply(msg: Any) {
    sender ! msg
  }

  private def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) = {
    val task = new LightReaction(this,
                                 if (f eq null) continuation else f,
                                 msg)
    scheduler execute task
  }

  def start(): OutputChannelActor = synchronized {
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
      throw new SuspendActorException
    }
    first
    throw new KillActorException
  }

  protected[actors] def exit(): Nothing = {
    terminated()
    throw new SuspendActorException
  }

  protected[actors] def terminated() {
    scheduler.terminated(this)
  }

}
