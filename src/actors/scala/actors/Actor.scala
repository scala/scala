/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.util.control.ControlException
import java.util.{Timer, TimerTask}
import java.util.concurrent.{ExecutionException, Callable}

/**
 * The <code>Actor</code> object provides functions for the definition of
 * actors, as well as actor operations, such as
 * <code>receive</code>, <code>react</code>, <code>reply</code>,
 * etc.
 *
 * @author Philipp Haller
 */
object Actor {

  private[actors] val tl = new ThreadLocal[Reactor]

  // timer thread runs as daemon
  private[actors] val timer = new Timer(true)

  private[actors] val suspendException = new SuspendActorException

  /**
   * Returns the currently executing actor. Should be used instead
   * of <code>this</code> in all blocks of code executed by
   * actors.
   *
   * @return returns the currently executing actor.
   */
  def self: Actor = self(Scheduler)

  private[actors] def self(sched: IScheduler): Actor =
    rawSelf(sched).asInstanceOf[Actor]

  private[actors] def rawSelf: Reactor = rawSelf(Scheduler)

  private[actors] def rawSelf(sched: IScheduler): Reactor = {
    val s = tl.get
    if (s eq null) {
      val r = new ActorProxy(currentThread, sched)
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
   * It replaces the implicit <code>ActorProxy</code> instance
   * of the current thread (if any) with a new instance.
   *
   * This permits to re-use the current thread as an actor
   * even if its <code>ActorProxy</code> has died for some reason.
   */
  def resetProxy {
    val a = tl.get
    if ((null ne a) && a.isInstanceOf[ActorProxy])
      tl.set(new ActorProxy(currentThread, parentScheduler))
  }

  /**
   * Removes any reference to an <code>Actor</code> instance
   * currently stored in thread-local storage.
   *
   * This allows to release references from threads that are
   * potentially long-running or being re-used (e.g. inside
   * a thread pool). Permanent references in thread-local storage
   * are a potential memory leak.
   */
  def clearSelf {
    tl.set(null)
  }

  /**
   * <p>This is a factory method for creating actors.</p>
   *
   * <p>The following example demonstrates its usage:</p>
   *
   * <pre>
   * import scala.actors.Actor._
   * ...
   * val a = actor {
   *   ...
   * }
   * </pre>
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
   * <p>
   * This is a factory method for creating actors whose
   * body is defined using a <code>Responder</code>.
   * </p>
   *
   * <p>The following example demonstrates its usage:</p>
   *
   * <pre>
   * import scala.actors.Actor._
   * import Responder.exec
   * ...
   * val a = reactor {
   *   for {
   *     res <- b !! MyRequest;
   *     if exec(println("result: "+res))
   *   } yield {}
   * }
   * </pre>
   *
   * @param  body  the <code>Responder</code> to be executed by the newly created actor
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
   * Receives the next message from the mailbox of the current actor
   * <code>self</code>.
   */
  def ? : Any = self.?

  /**
   * Receives a message from the mailbox of
   * <code>self</code>. Blocks if no message matching any of the
   * cases of <code>f</code> can be received.
   *
   * @param  f a partial function specifying patterns and actions
   * @return   the result of processing the received message
   */
  def receive[A](f: Any =>? A): A =
    self.receive(f)

  /**
   * Receives a message from the mailbox of
   * <code>self</code>. Blocks at most <code>msec</code>
   * milliseconds if no message matching any of the cases of
   * <code>f</code> can be received. If no message could be
   * received the <code>TIMEOUT</code> action is executed if
   * specified.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      the result of processing the received message
   */
  def receiveWithin[R](msec: Long)(f: Any =>? R): R =
    self.receiveWithin(msec)(f)

  /**
   * Lightweight variant of <code>receive</code>.
   *
   * Actions in <code>f</code> have to contain the rest of the
   * computation of <code>self</code>, as this method will never
   * return.
   *
   * @param  f a partial function specifying patterns and actions
   * @return   this function never returns
   */
  def react(f: Any =>? Unit): Nothing =
    rawSelf.react(f)

  /**
   * Lightweight variant of <code>receiveWithin</code>.
   *
   * Actions in <code>f</code> have to contain the rest of the
   * computation of <code>self</code>, as this method will never
   * return.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      this function never returns
   */
  def reactWithin(msec: Long)(f: Any =>? Unit): Nothing =
    self.reactWithin(msec)(f)

  def eventloop(f: Any =>? Unit): Nothing =
    rawSelf.react(new RecursiveProxyHandler(rawSelf, f))

  private class RecursiveProxyHandler(a: Reactor, f: Any =>? Unit)
          extends (Any =>? Unit) {
    def isDefinedAt(m: Any): Boolean =
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
    rawSelf.asInstanceOf[ReplyReactor].sender

  /**
   * Send <code>msg</code> to the actor waiting in a call to
   * <code>!?</code>.
   */
  def reply(msg: Any): Unit =
    rawSelf.asInstanceOf[ReplyReactor].reply(msg)

  /**
   * Send <code>()</code> to the actor waiting in a call to
   * <code>!?</code>.
   */
  def reply(): Unit =
    rawSelf.asInstanceOf[ReplyReactor].reply(())

  /**
   * Returns the number of messages in <code>self</code>'s mailbox
   *
   * @return the number of messages in <code>self</code>'s mailbox
   */
  def mailboxSize: Int = rawSelf.mailboxSize

  /**
   * <p>
   * Converts a synchronous event-based operation into
   * an asynchronous <code>Responder</code>.
   * </p>
   *
   * <p>The following example demonstrates its usage:</p>
   *
   * <pre>
   * val adder = reactor {
   *   for {
   *     _ <- respondOn(react) { case Add(a, b) => reply(a+b) }
   *   } yield {}
   * }
   * </pre>
   */
  def respondOn[A, B](fun: A =>? Unit => Nothing):
    A =>? B => Responder[B] =
      (caseBlock: A =>? B) => new Responder[B] {
        def respond(k: B => Unit) = fun(caseBlock andThen k)
      }

  private[actors] trait Body[a] {
    def andThen[b](other: => b): Unit
  }

  implicit def mkBody[a](body: => a) = new Body[a] {
    def andThen[b](other: => b): Unit = rawSelf.seq(body, other)
  }

  /**
   * Causes <code>self</code> to repeatedly execute
   * <code>body</code>.
   *
   * @param body the code block to be executed
   */
  def loop(body: => Unit): Unit = body andThen loop(body)

  /**
   * Causes <code>self</code> to repeatedly execute
   * <code>body</code> while the condition
   * <code>cond</code> is <code>true</code>.
   *
   * @param cond the condition to test
   * @param body the code block to be executed
   */
  def loopWhile(cond: => Boolean)(body: => Unit): Unit =
    if (cond) { body andThen loopWhile(cond)(body) }
    else continue

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param  to the actor to link to
   * @return    the parameter actor
   */
  def link(to: AbstractActor): AbstractActor = self.link(to)

  /**
   * Links <code>self</code> to the actor defined by <code>body</code>.
   *
   * @param body the body of the actor to link to
   * @return     the parameter actor
   */
  def link(body: => Unit): Actor = self.link(body)

  /**
   * Unlinks <code>self</code> from actor <code>from</code>.
   *
   * @param from the actor to unlink from
   */
  def unlink(from: Actor): Unit = self.unlink(from)

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
  def exit(reason: AnyRef): Nothing = self.exit(reason)

  /**
   * <p>
   *   Terminates execution of <code>self</code> with the following
   *   effect on linked actors:
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>true</code>, send message
   *   <code>Exit(self, 'normal)</code> to <code>a</code>.
   * </p>
   */
  def exit(): Nothing = self.exit()

  def continue: Unit = throw new KillActorException
}

/**
 * <p>
 *   This class provides an implementation of event-based actors.
 *   The main ideas of our approach are explained in the two papers
 * </p>
 * <ul>
 *   <li>
 *     <a href="http://lampwww.epfl.ch/~odersky/papers/jmlc06.pdf">
 *     <span style="font-weight:bold; white-space:nowrap;">Event-Based
 *     Programming without Inversion of Control</span></a>,<br/>
 *     Philipp Haller and Martin Odersky, <i>Proc. JMLC 2006</i>, and
 *   </li>
 *   <li>
 *     <a href="http://lamp.epfl.ch/~phaller/doc/haller07coord.pdf">
 *     <span style="font-weight:bold; white-space:nowrap;">Actors that
 *     Unify Threads and Events</span></a>,<br/>
 *     Philipp Haller and Martin Odersky, <i>Proc. COORDINATION 2007</i>.
 *   </li>
 * </ul>
 *
 * @author Philipp Haller
 */
@serializable @SerialVersionUID(-781154067877019505L)
trait Actor extends AbstractActor with ReplyReactor with ReplyableActor {

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

  /* This option holds a TimerTask when the actor waits in a
   * reactWithin/receiveWithin. The TimerTask is cancelled when
   * the actor can continue.
   */
  private var onTimeout: Option[TimerTask] = None

  protected[actors] override def scheduler: IScheduler = Scheduler

  private[actors] override def startSearch(msg: Any, replyTo: OutputChannel[Any], handler: Any => Boolean) =
    if (isSuspended) {
      () => synchronized {
        mailbox.append(msg, replyTo)
        resumeActor()
      }
    } else super.startSearch(msg, replyTo, handler)

  private[actors] override def makeReaction(fun: () => Unit): Runnable =
    new ActorTask(this, fun)

  private[actors] override def resumeReceiver(item: (Any, OutputChannel[Any]), onSameThread: Boolean) {
    synchronized {
      if (!onTimeout.isEmpty) {
        onTimeout.get.cancel()
        onTimeout = None
      }
    }
    senders = List(item._2)
    super.resumeReceiver(item, onSameThread)
  }

  /**
   * Receives a message from this actor's mailbox.
   *
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receive[R](f: Any =>? R): R = {
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
            waitingFor = f.isDefinedAt
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

  /**
   * Receives a message from this actor's mailbox within a certain
   * time span.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receiveWithin[R](msec: Long)(f: Any =>? R): R = {
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
        error("unhandled timeout")
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
            waitingFor = f.isDefinedAt
            received = None
            isSuspended = true
            val thisActor = this
            onTimeout = Some(new TimerTask {
              def run() { thisActor.send(TIMEOUT, thisActor) }
            })
            Actor.timer.schedule(onTimeout.get, msec)
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

  /**
   * Receives a message from this actor's mailbox.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  f    a partial function with message patterns and actions
   */
  override def react(f: Any =>? Unit): Nothing = {
    assert(Actor.self(scheduler) == this, "react on channel belonging to other actor")
    synchronized {
      if (shouldExit) exit() // links
      drainSendBuffer(mailbox)
    }
    continuation = f
    searchMailbox(mailbox, f.isDefinedAt, false)
    throw Actor.suspendException
  }

  /**
   * Receives a message from this actor's mailbox within a certain
   * time span.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   */
  def reactWithin(msec: Long)(f: Any =>? Unit): Nothing = {
    assert(Actor.self(scheduler) == this, "react on channel belonging to other actor")

    synchronized {
      if (shouldExit) exit() // links
      drainSendBuffer(mailbox)
    }

    // first, remove spurious TIMEOUT message from mailbox if any
    mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => m == TIMEOUT)

    val receiveTimeout = () => {
      if (f.isDefinedAt(TIMEOUT)) {
        senders = List(this)
        scheduleActor(f, TIMEOUT)
      } else
        error("unhandled timeout")
    }

    var done = false
    while (!done) {
      val qel = mailbox.extractFirst((m: Any, replyTo: OutputChannel[Any]) => {
        senders = List(replyTo)
        f.isDefinedAt(m)
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
            waitingFor = f.isDefinedAt
            continuation = f
            val thisActor = this
            onTimeout = Some(new TimerTask {
              def run() { thisActor.send(TIMEOUT, thisActor) }
            })
            Actor.timer.schedule(onTimeout.get, msec)
            done = true
            () => {}
          }
        }
        todo()
      } else {
        senders = List(qel.session)
        scheduleActor(f, qel.msg)
        done = true
      }
    }

    throw Actor.suspendException
  }

  /**
   * Receives the next message from this actor's mailbox.
   */
  def ? : Any = receive {
    case x => x
  }

  // guarded by lock of this
  // never throws SuspendActorException
  private[actors] override def scheduleActor(f: Any =>? Unit, msg: Any) =
    if ((f eq null) && (continuation eq null)) {
      // do nothing (timeout is handled instead)
    }
    else {
      val task = new Reaction(this,
                              if (f eq null) continuation else f,
                              msg)
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

  /**
   * Starts this actor.
   */
  override def start(): Actor = synchronized {
    // Reset various flags.
    //
    // Note that we do *not* reset `trapExit`. The reason is that
    // users should be able to set the field in the constructor
    // and before `act` is called.

    exitReason = 'normal
    exiting = false
    shouldExit = false

    scheduler.newActor(this)
    scheduler.execute(new Reaction(this))

    this
  }

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
   * Links <code>self</code> to the actor defined by <code>body</code>.
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
   * Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: AbstractActor) {
    assert(Actor.self(scheduler) == this, "unlink called on actor different from self")
    this unlinkFrom from
    from unlinkFrom this
  }

  private[actors] def unlinkFrom(from: AbstractActor) = synchronized {
    links = links.filterNot(from.==)
  }

  var trapExit = false
  private[actors] var exitReason: AnyRef = 'normal
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
    exitReason = reason
    exit()
  }

  /**
   * Terminates with exit reason <code>'normal</code>.
   */
  protected[actors] override def exit(): Nothing = {
    // links
    if (!links.isEmpty)
      exitLinked()
    terminated()
    throw Actor.suspendException
  }

  // Assume !links.isEmpty
  private[actors] def exitLinked() {
    exiting = true
    // remove this from links
    val mylinks = links.filterNot(this.==)
    // exit linked processes
    mylinks.foreach((linked: AbstractActor) => {
      unlink(linked)
      if (!linked.exiting)
        linked.exit(this, exitReason)
    })
  }

  // Assume !links.isEmpty
  private[actors] def exitLinked(reason: AnyRef) {
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
        else if (waitingFor ne waitingForNone) {
          scheduleActor(continuation, null)
          /* Here we should not throw a SuspendActorException,
             since the current method is called from an actor that
             is in the process of exiting.

             Therefore, the contract for scheduleActor is that
             it never throws a SuspendActorException.
           */
        }
      }
  }

  /* Requires qualified private, because <code>RemoteActor</code> must
   * register a termination handler.
   */
  private[actors] def onTerminate(f: => Unit) {
    scheduler.onTerminate(this) { f }
  }
}


/** <p>
 *    This object is used as the timeout pattern in
 *    <a href="Actor.html#receiveWithin(Long)" target="contentFrame">
 *    <code>receiveWithin</code></a> and
 *    <a href="Actor.html#reactWithin(Long)" target="contentFrame">
 *    <code>reactWithin</code></a>.
 *  </p>
 *  <p>
 *    The following example demonstrates its usage:
 *  </p><pre>
 *    receiveWithin(500) {
 *      <b>case</b> (x, y) <b>=&gt;</b> ...
 *      <b>case</b> TIMEOUT <b>=&gt;</b> ...
 *    }</pre>
 *
 *  @version 0.9.8
 *  @author Philipp Haller
 */
case object TIMEOUT


case class Exit(from: AbstractActor, reason: AnyRef)

/** <p>
 *    This class is used to manage control flow of actor
 *    executions.
 *  </p>
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
private[actors] class SuspendActorException extends Throwable with ControlException
