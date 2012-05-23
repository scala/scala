/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.actors
import java.util.TimerTask
import scala.util.control.ControlThrowable

private[actors] object InternalActor {
  private[actors] trait Body[a] {
    def andThen[b](other: => b): Unit
  }
}

private[actors] trait InternalActor extends AbstractActor with InternalReplyReactor with ActorCanReply with InputChannel[Any] with Serializable {

  /* The following two fields are only used when the actor
   * suspends by blocking its underlying thread, for example,
   * when waiting in a receive or synchronous send.
   */
  @volatile
  private[actors] var isSuspended = false

  /* This field is used to communicate the received message from
   * the invocation of send to the place where the thread of
   * the receiving actor resumes inside receive/receiveWithin.
   */
  @volatile
  private var received: Option[Any] = None

  protected[actors] override def scheduler: IScheduler = Scheduler

  private[actors] override def startSearch(msg: Any, replyTo: OutputChannel[Any], handler: PartialFunction[Any, Any]) =
    if (isSuspended) {
      () =>
        synchronized {
          mailbox.append(msg, replyTo)
          resumeActor()
        }
    } else super.startSearch(msg, replyTo, handler)

  // we override this method to check `shouldExit` before suspending
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
            // very important to check for `shouldExit` at this point
            // since linked actors might have set it after we checked
            // last time (e.g., at the beginning of `react`)
            if (shouldExit) exit()
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

  private[actors] override def makeReaction(fun: () => Unit, handler: PartialFunction[Any, Any], msg: Any): Runnable =
    new ActorTask(this, fun, handler, msg)

  /** See the companion object's `receive` method. */
  def receive[R](f: PartialFunction[Any, R]): R = {
    assert(Actor.self(scheduler) == this, "receive from channel belonging to other actor")

    synchronized {
      if (shouldExit) exit() // links
      drainSendBuffer(mailbox)
    }

    var done = false
    while (!done) {
      val qel = mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => {
        senders = replyTo :: senders
        val matches = f.isDefinedAt(m)
        senders = senders.tail
        matches
      })
      if (null eq qel) {
        synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            drainSendBuffer(mailbox)
            // keep going
          } else {
            waitingFor = f
            isSuspended = true
            scheduler.managedBlock(blocker)
            drainSendBuffer(mailbox)
            // keep going
          }
        }
      } else {
        received = Some(qel.msg)
        senders = qel.session :: senders
        done = true
      }
    }

    val result = f(received.get)
    received = None
    senders = senders.tail
    result
  }

  /** See the companion object's `receiveWithin` method. */
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self(scheduler) == this, "receive from channel belonging to other actor")

    synchronized {
      if (shouldExit) exit() // links
      drainSendBuffer(mailbox)
    }

    // first, remove spurious TIMEOUT message from mailbox if any
    mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => m == TIMEOUT)

    val receiveTimeout = () => {
      if (f.isDefinedAt(TIMEOUT)) {
        received = Some(TIMEOUT)
        senders = this :: senders
      } else
        sys.error("unhandled timeout")
    }

    var done = false
    while (!done) {
      val qel = mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => {
        senders = replyTo :: senders
        val matches = f.isDefinedAt(m)
        senders = senders.tail
        matches
      })
      if (null eq qel) {
        val todo = synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            drainSendBuffer(mailbox)
            // keep going
            () => {}
          } else if (msec == 0L) {
            done = true
            receiveTimeout
          } else {
            if (onTimeout.isEmpty) {
              if (!f.isDefinedAt(TIMEOUT))
                sys.error("unhandled timeout")

              val thisActor = this
              onTimeout = Some(new TimerTask {
                def run() {
                  thisActor.send(TIMEOUT, thisActor)
                }
              })
              Actor.timer.schedule(onTimeout.get, msec)
            }

            // It is possible that !onTimeout.isEmpty, but TIMEOUT is not yet in mailbox
            // See SI-4759
            waitingFor = f
            received = None
            isSuspended = true
            scheduler.managedBlock(blocker)
            drainSendBuffer(mailbox)
            // keep going
            () => {}
          }
        }
        todo()
      } else {
        synchronized {
          if (!onTimeout.isEmpty) {
            onTimeout.get.cancel()
            onTimeout = None
          }
        }
        received = Some(qel.msg)
        senders = qel.session :: senders
        done = true
      }
    }

    val result = f(received.get)
    received = None
    senders = senders.tail
    result
  }

  /** See the companion object's `react` method. */
  override def react(handler: PartialFunction[Any, Unit]): Nothing = {
    synchronized {
      if (shouldExit) exit()
    }
    super.react(handler)
  }

  /** See the companion object's `reactWithin` method. */
  override def reactWithin(msec: Long)(handler: PartialFunction[Any, Unit]): Nothing = {
    synchronized {
      if (shouldExit) exit()
    }
    super.reactWithin(msec)(handler)
  }

  /** Receives the next message from the mailbox */
  def ? : Any = receive {
    case x => x
  }

  // guarded by lock of this
  // never throws SuspendActorControl
  private[actors] override def scheduleActor(f: PartialFunction[Any, Any], msg: Any) =
    if (f eq null) {
      // do nothing (timeout is handled instead)
    } else {
      val task = new ActorTask(this, null, f, msg)
      scheduler executeFromActor task
    }

  /* Used for notifying scheduler when blocking inside receive/receiveWithin. */
  private object blocker extends scala.concurrent.ManagedBlocker {
    def block() = {
      InternalActor.this.suspendActor()
      true
    }
    def isReleasable =
      !InternalActor.this.isSuspended
  }

  private def suspendActor() = synchronized {
    while (isSuspended) {
      try {
        wait()
      } catch {
        case _: InterruptedException =>
      }
    }
    // links: check if we should exit
    if (shouldExit) exit()
  }

  private def resumeActor() {
    isSuspended = false
    notify()
  }

  private[actors] override def exiting = synchronized {
    _state == Actor.State.Terminated
  }

  // guarded by this
  private[actors] override def dostart() {
    // Reset various flags.
    //
    // Note that we do *not* reset `trapExit`. The reason is that
    // users should be able to set the field in the constructor
    // and before `act` is called.
    exitReason = 'normal
    shouldExit = false

    super.dostart()
  }

  override def start(): InternalActor = synchronized {
    super.start()
    this
  }

  /** State of this actor */
  override def getState: Actor.State.Value = synchronized {
    if (isSuspended) {
      if (onTimeout.isEmpty)
        Actor.State.Blocked
      else
        Actor.State.TimedBlocked
    } else
      super.getState
  }

  // guarded by this
  private[actors] var links: List[AbstractActor] = Nil

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param to the actor to link to
   * @return   the parameter actor
   */
  def link(to: AbstractActor): AbstractActor = {
    assert(Actor.self(scheduler) == this, "link called on actor different from self")
    this linkTo to
    to linkTo this
    to
  }

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param to the actor to link to
   * @return   the parameter actor
   */
  def link(to: ActorRef): ActorRef = {
    this.link(to.localActor)
    to
  }

  /**
   *  Unidirectional linking. For migration purposes only
   */
  private[actors] def watch(subject: ActorRef): ActorRef = {
    assert(Actor.self(scheduler) == this, "link called on actor different from self")
    subject.localActor linkTo this
    subject
  }

  /**
   *  Unidirectional linking. For migration purposes only
   */
  private[actors] def unwatch(subject: ActorRef): ActorRef = {
    assert(Actor.self(scheduler) == this, "link called on actor different from self")
    subject.localActor unlinkFrom this
    subject
  }

  /**
   * Links <code>self</code> to the actor defined by <code>body</code>.
   *
   * @param body the body of the actor to link to
   * @return     the parameter actor
   */
  def link(body: => Unit): Actor = {
    assert(Actor.self(scheduler) == this, "link called on actor different from self")
    val a = new Actor {
      def act() = body
      override final val scheduler: IScheduler = InternalActor.this.scheduler
    }
    link(a)
    a.start()
    a
  }

  private[actors] def linkTo(to: AbstractActor) = synchronized {
    links = to :: links
  }

  /**
   * Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: AbstractActor) {
    assert(Actor.self(scheduler) == this, "unlink called on actor different from self")
    this unlinkFrom from
    from unlinkFrom this
  }

  /**
   * Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: ActorRef) {
    unlink(from.localActor)
  }

  private[actors] def unlinkFrom(from: AbstractActor) = synchronized {
    links = links.filterNot(from.==)
  }

  @volatile
  private[actors] var _trapExit = false

  def trapExit = _trapExit

  def trapExit_=(value: Boolean) = _trapExit = value

  // guarded by this
  private var exitReason: AnyRef = 'normal
  // guarded by this
  private[actors] var shouldExit = false

  /**
   * <p>
   *   Terminates execution of <code>self</code> with the following
   *   effect on linked actors:
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>true</code>, send message
   *   <code>Exit(self, reason)</code> to <code>a</code>.
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>false</code> (default),
   *   call <code>a.exit(reason)</code> if
   *   <code>reason != 'normal</code>.
   * </p>
   */
  protected[actors] def exit(reason: AnyRef): Nothing = {
    synchronized {
      exitReason = reason
    }
    exit()
  }

  /**
   * Terminates with exit reason <code>'normal</code>.
   */
  protected[actors] override def exit(): Nothing = {
    val todo = synchronized {
      if (!links.isEmpty)
        exitLinked()
      else
        () => {}
    }
    todo()
    super.exit()
  }

  // Assume !links.isEmpty
  // guarded by this
  private[actors] def exitLinked(): () => Unit = {
    _state = Actor.State.Terminated
    // reset waitingFor, otherwise getState returns Suspended
    waitingFor = Reactor.waitingForNone
    // remove this from links
    val mylinks = links.filterNot(this.==)
    // unlink actors
    mylinks.foreach(unlinkFrom(_))
    // return closure that locks linked actors
    () => {
      mylinks.foreach((linked: AbstractActor) => {
        linked.synchronized {
          if (!linked.exiting) {
            linked.unlinkFrom(this)
            linked.exit(this, exitReason)
          }
        }
      })
    }
  }

  // Assume !links.isEmpty
  // guarded by this
  private[actors] def exitLinked(reason: AnyRef): () => Unit = {
    exitReason = reason
    exitLinked()
  }

  // Assume !this.exiting
  private[actors] def exit(from: AbstractActor, reason: AnyRef) {
    if (trapExit) {
      this ! Exit(from, reason)
    } else if (reason != 'normal)
      stop(reason)
  }

  /* Requires qualified private, because <code>RemoteActor</code> must
   * register a termination handler.
   */
  private[actors] def onTerminate(f: => Unit) {
    scheduler.onTerminate(this) { f }
  }


  private[actors] def stop(reason: AnyRef): Unit = {
    synchronized {
      shouldExit = true
      exitReason = reason
      // resume this Actor in a way that
      // causes it to exit
      // (because shouldExit == true)
      if (isSuspended)
        resumeActor()
      else if (waitingFor ne Reactor.waitingForNone) {
        waitingFor = Reactor.waitingForNone
        // it doesn't matter what partial function we are passing here
        val task = new ActorTask(this, null, waitingFor, null)
        scheduler execute task
        /* Here we should not throw a SuspendActorControl,
           since the current method is called from an actor that
           is in the process of exiting.

           Therefore, the contract for scheduleActor is that
           it never throws a SuspendActorControl.
         */
      }
    }
  }
}

/**
 *    Used as the timeout pattern in
 *    <a href="Actor.html#receiveWithin(Long)" target="contentFrame">
 *    <code>receiveWithin</code></a> and
 *    <a href="Actor.html#reactWithin(Long)" target="contentFrame">
 *    <code>reactWithin</code></a>.
 *
 * @example {{{
 *    receiveWithin(500) {
 *      case (x, y) => ...
 *      case TIMEOUT => ...
 *    }
 * }}}
 *
 *  @author Philipp Haller
 */
case object TIMEOUT

/**
 * Sent to an actor
 *  with `trapExit` set to `true` whenever one of its linked actors
 *  terminates.
 *
 *  @param from   the actor that terminated
 *  @param reason the reason that caused the actor to terminate
 */
case class Exit(from: AbstractActor, reason: AnyRef)

/**
 * Manages control flow of actor executions.
 *
 * @author Philipp Haller
 */
private[actors] class SuspendActorControl extends ControlThrowable
