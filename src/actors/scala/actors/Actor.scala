/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scala.util.control.ControlThrowable
import java.util.{Timer, TimerTask}

/**
 * Provides functions for the definition of actors, as well as actor
 * operations, such as `receive`, `react`, `reply`, etc.
 *
 * @author Philipp Haller
 */
object Actor extends Combinators {

  /** State of an actor.
   *
   *  - '''New''' -
   *      Not yet started
   *  - '''Runnable''' -
   *      Executing
   *  - '''Suspended''' -
   *      Suspended, waiting in a `react`
   *  - '''TimedSuspended''' -
   *      Suspended, waiting in a `reactWithin`
   *  - '''Blocked''' -
   *      Blocked waiting in a `receive`
   *  - '''TimedBlocked''' -
   *      Blocked waiting in a `receiveWithin`
   *  - '''Terminated''' -
   *      Actor has terminated
   */
  object State extends Enumeration {
    val New,
        Runnable,
        Suspended,
        TimedSuspended,
        Blocked,
        TimedBlocked,
        Terminated = Value
  }

  private[actors] val tl = new ThreadLocal[ReplyReactor]

  // timer thread runs as daemon
  private[actors] val timer = new Timer(true)

  private[actors] val suspendException = new SuspendActorControl

  /**
   * Returns the currently executing actor. Should be used instead
   * of `'''this'''` in all blocks of code executed by actors.
   *
   * @return returns the currently executing actor.
   */
  def self: Actor = self(Scheduler)

  private[actors] def self(sched: IScheduler): Actor =
    rawSelf(sched).asInstanceOf[Actor]

  private[actors] def rawSelf: ReplyReactor =
    rawSelf(Scheduler)

  private[actors] def rawSelf(sched: IScheduler): ReplyReactor = {
    val s = tl.get
    if (s eq null) {
      val r = new ActorProxy(Thread.currentThread, sched)
      tl.set(r)
      r
    } else
      s
  }

  private def parentScheduler: IScheduler = {
    val s = tl.get
    if (s eq null) Scheduler else s.scheduler
  }

  /**
   * Resets an actor proxy associated with the current thread.
   * It replaces the implicit `ActorProxy` instance
   * of the current thread (if any) with a new instance.
   *
   * This permits to re-use the current thread as an actor
   * even if its `ActorProxy` has died for some reason.
   */
  def resetProxy() {
    val a = tl.get
    if ((null ne a) && a.isInstanceOf[ActorProxy])
      tl.set(new ActorProxy(Thread.currentThread, parentScheduler))
  }

  /**
   * Removes any reference to an `Actor` instance
   * currently stored in thread-local storage.
   *
   * This allows to release references from threads that are potentially
   * long-running or being re-used (e.g. inside a thread pool). Permanent
   * references in thread-local storage are a potential memory leak.
   */
  def clearSelf() {
    tl set null
  }

  /**
   * Factory method for creating and starting an actor.
   *
   * @example {{{
   * import scala.actors.Actor._
   * ...
   * val a = actor {
   *   ...
   * }
   * }}}
   *
   * @param  body  the code block to be executed by the newly created actor
   * @return       the newly created actor. Note that it is automatically started.
   */
  def actor(body: => Unit): Actor = {
    val a = new Actor {
      def act() = body
      override final val scheduler: IScheduler = parentScheduler
    }
    a.start()
    a
  }

  /**
   * Factory method for creating actors whose
   * body is defined using a `Responder`.
   *
   * @example {{{
   * import scala.actors.Actor._
   * import Responder.exec
   * ...
   * val a = reactor {
   *   for {
   *     res <- b !! MyRequest;
   *     if exec(println("result: "+res))
   *   } yield {}
   * }
   * }}}
   *
   * @param  body  the `Responder` to be executed by the newly created actor
   * @return       the newly created actor. Note that it is automatically started.
   */
  def reactor(body: => Responder[Unit]): Actor = {
    val a = new Actor {
      def act() {
        Responder.run(body)
      }
      override final val scheduler: IScheduler = parentScheduler
    }
    a.start()
    a
  }

  /**
   * Receives the next message from the mailbox of the current actor `self`.
   */
  def ? : Any = self.?

  /**
   * Receives a message from the mailbox of `self`. Blocks if no message
   * matching any of the cases of `f` can be received.
   *
   * @example {{{
   * receive {
   *   case "exit" => println("exiting")
   *   case 42 => println("got the answer")
   *   case x:Int => println("got an answer")
   * }
   * }}}
   *
   * @param  f a partial function specifying patterns and actions
   * @return   the result of processing the received message
   */
  def receive[A](f: PartialFunction[Any, A]): A =
    self.receive(f)

  /**
   * Receives a message from the mailbox of `self`. Blocks at most `msec`
   * milliseconds if no message matching any of the cases of `f` can be
   * received. If no message could be received the `TIMEOUT` action is
   * executed if specified.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      the result of processing the received message
   */
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R =
    self.receiveWithin(msec)(f)

  /**
   * Lightweight variant of `receive`.
   *
   * Actions in `f` have to contain the rest of the computation of `self`,
   * as this method will never return.
   *
   * A common method of continuting the computation is to send a message
   * to another actor:
   * {{{
   * react {
   *   case Get(from) =>
   *     react {
   *       case Put(x) => from ! x
   *     }
   * }
   * }}}
   *
   * Another common method is to use `loop` to continuously `react` to messages:
   * {{{
   * loop {
   *   react {
   *     case Msg(data) => // process data
   *   }
   * }
   * }}}
   *
   * @param  f a partial function specifying patterns and actions
   * @return   this function never returns
   */
  def react(f: PartialFunction[Any, Unit]): Nothing =
    rawSelf.react(f)

  /**
   * Lightweight variant of `receiveWithin`.
   *
   * Actions in `f` have to contain the rest of the computation of `self`,
   * as this method will never return.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      this function never returns
   */
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing =
    self.reactWithin(msec)(f)

  def eventloop(f: PartialFunction[Any, Unit]): Nothing =
    rawSelf.react(new RecursiveProxyHandler(rawSelf, f))

  private class RecursiveProxyHandler(a: ReplyReactor, f: PartialFunction[Any, Unit])
          extends scala.runtime.AbstractPartialFunction[Any, Unit] {
    def _isDefinedAt(m: Any): Boolean =
      true // events are immediately removed from the mailbox
    def apply(m: Any) {
      if (f.isDefinedAt(m)) f(m)
      a.react(this)
    }
  }

  /**
   * Returns the actor which sent the last received message.
   */
  def sender: OutputChannel[Any] =
    rawSelf.sender

  /**
   * Sends `msg` to the actor waiting in a call to `!?`.
   */
  def reply(msg: Any): Unit =
    rawSelf.reply(msg)

  /**
   * Sends `()` to the actor waiting in a call to `!?`.
   */
  def reply(): Unit =
    rawSelf.reply(())

  /**
   * Returns the number of messages in `self`'s mailbox
   *
   * @return the number of messages in `self`'s mailbox
   */
  def mailboxSize: Int = rawSelf.mailboxSize

  /**
   * Converts a synchronous event-based operation into
   * an asynchronous `Responder`.
   *
   * @example {{{
   * val adder = reactor {
   *   for {
   *     _ <- respondOn(react) { case Add(a, b) => reply(a+b) }
   *   } yield {}
   * }
   * }}}
   */
  def respondOn[A, B](fun: PartialFunction[A, Unit] => Nothing):
    PartialFunction[A, B] => Responder[B] =
      (caseBlock: PartialFunction[A, B]) => new Responder[B] {
        def respond(k: B => Unit) = fun(caseBlock andThen k)
      }

  private[actors] trait Body[a] {
    def andThen[b](other: => b): Unit
  }

  implicit def mkBody[a](body: => a) = new Body[a] {
    def andThen[b](other: => b): Unit = rawSelf.seq(body, other)
  }

  /**
   * Links `self` to actor `to`.
   *
   * @param  to the actor to link to
   * @return    the parameter actor
   */
  def link(to: AbstractActor): AbstractActor = self.link(to)

  /**
   * Links `self` to the actor defined by `body`.
   *
   * @param body the body of the actor to link to
   * @return     the parameter actor
   */
  def link(body: => Unit): Actor = self.link(body)

  /**
   * Unlinks `self` from actor `from`.
   *
   * @param from the actor to unlink from
   */
  def unlink(from: AbstractActor): Unit = self.unlink(from)

  /**
   * Terminates execution of `self` with the following effect on
   * linked actors:
   *
   * For each linked actor `a` with `trapExit` set to `'''true'''`,
   * send message `Exit(self, reason)` to `a`.
   *
   * For each linked actor `a` with `trapExit` set to `'''false'''`
   * (default), call `a.exit(reason)` if `reason != 'normal`.
   */
  def exit(reason: AnyRef): Nothing = self.exit(reason)

  /**
   * Terminates execution of `self` with the following effect on
   * linked actors:
   *
   * For each linked actor `a` with `trapExit` set to `'''true'''`,
   * send message `Exit(self, 'normal)` to `a`.
   */
  def exit(): Nothing = rawSelf.exit()

}

/** Provides lightweight, concurrent actors. Actors are created by extending
 *  the `Actor` trait (alternatively, one of the factory methods in its
 *  companion object can be used).  The behavior of an `Actor` subclass is
 *  defined by implementing its `act` method:
 *  {{{
 *  class MyActor extends Actor {
 *    def act() {
 *      // actor behavior goes here
 *    }
 *  }
 *  }}}
 *  A new `Actor` instance is started by invoking its `start` method.
 *
 *  '''Note:''' care must be taken when invoking thread-blocking methods other
 *  than those provided by the `Actor` trait or its companion object (such as
 *  `receive`). Blocking the underlying thread inside an actor may lead to
 *  starvation of other actors. This also applies to actors hogging their
 *  thread for a long time between invoking `receive`/`react`.
 *
 *  If actors use blocking operations (for example, methods for blocking I/O),
 *  there are several options:
 *
 *  - The run-time system can be configured to use a larger thread pool size
 *    (for example, by setting the `actors.corePoolSize` JVM property).
 *  - The `scheduler` method of the `Actor` trait can be overridden to return a
 *    `ResizableThreadPoolScheduler`, which resizes its thread pool to
 *    avoid starvation caused by actors that invoke arbitrary blocking methods.
 *  - The `actors.enableForkJoin` JVM property can be set to `false`, in which
 *    case a `ResizableThreadPoolScheduler` is used by default to execute actors.
 *
 *  The main ideas of the implementation are explained in the two papers
 *
 *  - [[http://lampwww.epfl.ch/~odersky/papers/jmlc06.pdf Event-Based
 *    Programming without Inversion of Control]],
 *    Philipp Haller and Martin Odersky, ''Proc. JMLC 2006'', and
 *  - [[http://lamp.epfl.ch/~phaller/doc/haller07coord.pdf Actors that
 *    Unify Threads and Events]],
 *    Philipp Haller and Martin Odersky, ''Proc. COORDINATION 2007''.
 *
 *  @author Philipp Haller
 *
 *  @define actor actor
 *  @define channel actor's mailbox
 */
@SerialVersionUID(-781154067877019505L)
trait Actor extends AbstractActor with ReplyReactor with ActorCanReply with InputChannel[Any] with Serializable {

  /* The following two fields are only used when the actor
   * suspends by blocking its underlying thread, for example,
   * when waiting in a receive or synchronous send.
   */
  @volatile
  private var isSuspended = false

  /* This field is used to communicate the received message from
   * the invocation of send to the place where the thread of
   * the receiving actor resumes inside receive/receiveWithin.
   */
  @volatile
  private var received: Option[Any] = None

  protected[actors] override def scheduler: IScheduler = Scheduler

  private[actors] override def startSearch(msg: Any, replyTo: OutputChannel[Any], handler: PartialFunction[Any, Any]) =
    if (isSuspended) {
      () => synchronized {
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
    }
    else {
      val task = new ActorTask(this, null, f, msg)
      scheduler executeFromActor task
    }

  /* Used for notifying scheduler when blocking inside receive/receiveWithin. */
  private object blocker extends scala.concurrent.ManagedBlocker {
    def block() = {
      Actor.this.suspendActor()
      true
    }
    def isReleasable =
      !Actor.this.isSuspended
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

  override def start(): Actor = synchronized {
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
   * Links `self` to actor `to`.
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
   * Links `self` to the actor defined by `body`.
   *
   * @param body the body of the actor to link to
   * @return     the parameter actor
   */
  def link(body: => Unit): Actor = {
    assert(Actor.self(scheduler) == this, "link called on actor different from self")
    val a = new Actor {
      def act() = body
      override final val scheduler: IScheduler = Actor.this.scheduler
    }
    link(a)
    a.start()
    a
  }

  private[actors] def linkTo(to: AbstractActor) = synchronized {
    links = to :: links
  }

  /**
   * Unlinks `self` from actor `from`.
   */
  def unlink(from: AbstractActor) {
    assert(Actor.self(scheduler) == this, "unlink called on actor different from self")
    this unlinkFrom from
    from unlinkFrom this
  }

  private[actors] def unlinkFrom(from: AbstractActor) = synchronized {
    links = links.filterNot(from.==)
  }

  @volatile
  var trapExit = false
  // guarded by this
  private var exitReason: AnyRef = 'normal
  // guarded by this
  private[actors] var shouldExit = false

  /**
   * Terminates execution of `self` with the following effect on
   * linked actors:
   *
   * For each linked actor `a` with `trapExit` set to `'''true'''`,
   * send message `Exit(self, reason)` to `a`.
   *
   * For each linked actor `a` with `trapExit` set to `'''false'''`
   * (default), call `a.exit(reason)` if `reason != 'normal`.
   */
  protected[actors] def exit(reason: AnyRef): Nothing = {
    synchronized {
      exitReason = reason
    }
    exit()
  }

  /**
   * Terminates with exit reason `'normal`.
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
    }
    else if (reason != 'normal)
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
          scheduleActor(waitingFor, null)
          /* Here we should not throw a SuspendActorControl,
             since the current method is called from an actor that
             is in the process of exiting.

             Therefore, the contract for scheduleActor is that
             it never throws a SuspendActorControl.
           */
        }
      }
  }

  /** Requires qualified private, because `RemoteActor` must
   * register a termination handler.
   */
  private[actors] def onTerminate(f: => Unit) {
    scheduler.onTerminate(this) { f }
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


/** Sent to an actor with `trapExit` set to `'''true'''` whenever one of its
 *  linked actors terminates.
 *
 *  @param from   the actor that terminated
 *  @param reason the reason that caused the actor to terminate
 */
case class Exit(from: AbstractActor, reason: AnyRef)

/** Manages control flow of actor executions.
 *
 * @author Philipp Haller
 */
private[actors] class SuspendActorControl extends ControlThrowable
