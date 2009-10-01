/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.{HashSet, Queue}
import scala.compat.Platform

import java.util.{Timer, TimerTask}

/**
 * The <code>Actor</code> object provides functions for the definition of
 * actors, as well as actor operations, such as
 * <code>receive</code>, <code>react</code>, <code>reply</code>,
 * etc.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
object Actor {

  private[actors] val tl = new ThreadLocal[Actor]

  // timer thread runs as daemon
  private[actors] val timer = new Timer(true)

  /**
   * Returns the currently executing actor. Should be used instead
   * of <code>this</code> in all blocks of code executed by
   * actors.
   *
   * @return returns the currently executing actor.
   */
  def self: Actor = {
    var a = tl.get.asInstanceOf[Actor]
    if (null eq a) {
      a = new ActorProxy(currentThread)
      tl.set(a)
    }
    a
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
    val a = tl.get.asInstanceOf[Actor]
    if ((null ne a) && a.isInstanceOf[ActorProxy])
      tl.set(new ActorProxy(currentThread))
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
    val actor = new Actor {
      def act() = body
    }
    actor.start()
    actor
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
  def receive[A](f: PartialFunction[Any, A]): A =
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
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R =
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
  def react(f: PartialFunction[Any, Unit]): Nothing =
    self.react(f)

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
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing =
    self.reactWithin(msec)(f)

  def eventloop(f: PartialFunction[Any, Unit]): Nothing =
    self.react(new RecursiveProxyHandler(self, f))

  private class RecursiveProxyHandler(a: Actor, f: PartialFunction[Any, Unit])
          extends PartialFunction[Any, Unit] {
    def isDefinedAt(m: Any): Boolean =
      true // events are immediately removed from the mailbox
    def apply(m: Any) {
      if (f.isDefinedAt(m)) f(m)
      self.react(this)
    }
  }

  /**
   * Returns the actor which sent the last received message.
   */
  def sender: OutputChannel[Any] = self.sender

  /**
   * Send <code>msg</code> to the actor waiting in a call to
   * <code>!?</code>.
   */
  def reply(msg: Any): Unit = self.reply(msg)

  /**
   * Send <code>()</code> to the actor waiting in a call to
   * <code>!?</code>.
   */
  def reply(): Unit = self.reply(())

  /**
   * Returns the number of messages in <code>self</code>'s mailbox
   *
   * @return the number of messages in <code>self</code>'s mailbox
   */
  def mailboxSize: Int = self.mailboxSize

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
  def respondOn[A, B](fun: PartialFunction[A, Unit] => Nothing):
    PartialFunction[A, B] => Responder[B] =
      (caseBlock: PartialFunction[A, B]) => new Responder[B] {
        def respond(k: B => Unit) = fun(caseBlock andThen k)
      }

  private[actors] trait Body[a] {
    def andThen[b](other: => b): Unit
  }

  implicit def mkBody[a](body: => a) = new Body[a] {
    def andThen[b](other: => b): Unit = self.seq(body, other)
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
   * @return
   */
  def link(to: AbstractActor): AbstractActor = self.link(to)

  /**
   * Links <code>self</code> to actor defined by <code>body</code>.
   *
   * @param body ...
   * @return     ...
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
 * @version 0.9.18
 * @author Philipp Haller
 */
@serializable
trait Actor extends AbstractActor {

  private var received: Option[Any] = None

  private val waitingForNone = (m: Any) => false
  private var waitingFor: Any => Boolean = waitingForNone
  private var isSuspended = false

  protected val mailbox = new MessageQueue
  private var sessions: List[OutputChannel[Any]] = Nil

  protected def scheduler: IScheduler =
    Scheduler

  /**
   * Returns the number of messages in this actor's mailbox
   *
   * @return the number of messages in this actor's mailbox
   */
  def mailboxSize: Int = synchronized {
    mailbox.size
  }

  /**
   * Sends <code>msg</code> to this actor (asynchronous) supplying
   * explicit reply destination.
   *
   * @param  msg      the message to send
   * @param  replyTo  the reply destination
   */
  def send(msg: Any, replyTo: OutputChannel[Any]) = synchronized {
    if (waitingFor(msg)) {
      received = Some(msg)

      if (isSuspended)
        sessions = replyTo :: sessions
      else
        sessions = List(replyTo)

      waitingFor = waitingForNone

      if (!onTimeout.isEmpty) {
        onTimeout.get.cancel()
        onTimeout = None
      }

      if (isSuspended)
        resumeActor()
      else // assert continuation != null
        scheduler.execute(new Reaction(this, continuation, msg))
    } else {
      mailbox.append(msg, replyTo)
    }
  }

  /**
   * Receives a message from this actor's mailbox.
   *
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receive[R](f: PartialFunction[Any, R]): R = {
    assert(Actor.self == this, "receive from channel belonging to other actor")
    this.synchronized {
      if (shouldExit) exit() // links
      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        waitingFor = f.isDefinedAt
        isSuspended = true
        suspendActor()
      } else {
        received = Some(qel.msg)
        sessions = qel.session :: sessions
      }
      waitingFor = waitingForNone
      isSuspended = false
    }
    val result = f(received.get)
    sessions = sessions.tail
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
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self == this, "receive from channel belonging to other actor")
    this.synchronized {
      if (shouldExit) exit() // links

      // first, remove spurious TIMEOUT message from mailbox if any
      val spurious = mailbox.extractFirst((m: Any) => m == TIMEOUT)

      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        if (msec == 0) {
          if (f.isDefinedAt(TIMEOUT))
            return f(TIMEOUT)
          else
            error("unhandled timeout")
        }
        else {
          waitingFor = f.isDefinedAt
          isSuspended = true
          received = None
          suspendActorFor(msec)
          if (received.isEmpty) {
            if (f.isDefinedAt(TIMEOUT)) {
              waitingFor = waitingForNone
              isSuspended = false
              val result = f(TIMEOUT)
              return result
            }
            else
              error("unhandled timeout")
          }
        }
      } else {
        received = Some(qel.msg)
        sessions = qel.session :: sessions
      }
      waitingFor = waitingForNone
      isSuspended = false
    }
    val result = f(received.get)
    sessions = sessions.tail
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
  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == this, "react on channel belonging to other actor")
    this.synchronized {
      if (shouldExit) exit() // links
      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        waitingFor = f.isDefinedAt
        continuation = f
        isDetached = true
      } else {
        sessions = List(qel.session)
        scheduleActor(f, qel.msg)
      }
      throw new SuspendActorException
    }
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
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == this, "react on channel belonging to other actor")
    this.synchronized {
      if (shouldExit) exit() // links

      // first, remove spurious TIMEOUT message from mailbox if any
      val spurious = mailbox.extractFirst((m: Any) => m == TIMEOUT)

      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        if (msec == 0) {
          if (f.isDefinedAt(TIMEOUT)) {
            sessions = List(Actor.self)
            scheduleActor(f, TIMEOUT)
          }
          else
            error("unhandled timeout")
        }
        else {
          waitingFor = f.isDefinedAt
          val thisActor = this
          onTimeout = Some(new TimerTask {
            def run() { thisActor ! TIMEOUT }
          })
          Actor.timer.schedule(onTimeout.get, msec)
          continuation = f
          isDetached = true
        }
      } else {
        sessions = List(qel.session)
        scheduleActor(f, qel.msg)
      }
      throw new SuspendActorException
    }
  }

  /**
   * The behavior of an actor is specified by implementing this
   * abstract method. Note that the preferred way to create actors
   * is through the <code>actor</code> method
   * defined in object <code>Actor</code>.
   */
  def act(): Unit

  /**
   * Sends <code>msg</code> to this actor (asynchronous).
   */
  def !(msg: Any) {
    send(msg, Actor.self)
  }

  /**
   * Forwards <code>msg</code> to this actor (asynchronous).
   */
  def forward(msg: Any) {
    send(msg, Actor.sender)
  }

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  def !?(msg: Any): Any = {
    val replyCh = Actor.self.freshReplyChannel
    send(msg, replyCh)
    replyCh.receive {
      case x => x
    }
  }

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
    val replyCh = Actor.self.freshReplyChannel
    send(msg, replyCh)
    replyCh.receiveWithin(msec) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   */
  def !!(msg: Any): Future[Any] = {
    val ftch = new Channel[Any](Actor.self)
    send(msg, ftch)
    new Future[Any](ftch) {
      def apply() =
        if (isSet) value.get
        else ch.receive {
          case any => value = Some(any); any
        }
      def respond(k: Any => Unit): Unit =
 	if (isSet) k(value.get)
 	else ch.react {
 	  case any => value = Some(any); k(any)
 	}
      def isSet = value match {
        case None => ch.receiveWithin(0) {
          case TIMEOUT => false
          case any => value = Some(any); true
        }
        case Some(_) => true
      }
    }
  }

  /**
   * Sends <code>msg</code> to this actor and immediately
   * returns a future representing the reply value.
   * The reply is post-processed using the partial function
   * <code>f</code>. This also allows to recover a more
   * precise type for the reply value.
   */
  def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A] = {
    val ftch = new Channel[Any](Actor.self)
    send(msg, ftch)
    new Future[A](ftch) {
      def apply() =
        if (isSet) value.get.asInstanceOf[A]
        else ch.receive {
          case any => value = Some(f(any)); value.get.asInstanceOf[A]
        }
      def respond(k: A => Unit): Unit =
 	if (isSet) k(value.get.asInstanceOf[A])
 	else ch.react {
 	  case any => value = Some(f(any)); k(value.get.asInstanceOf[A])
 	}
      def isSet = value match {
        case None => ch.receiveWithin(0) {
          case TIMEOUT => false
          case any => value = Some(f(any)); true
        }
        case Some(_) => true
      }
    }
  }

  /**
   * Replies with <code>msg</code> to the sender.
   */
  def reply(msg: Any) {
    sender ! msg
  }

  private var rc: Channel[Any] = null
  private[actors] def replyChannel = rc
  private[actors] def freshReplyChannel: Channel[Any] =
    { rc = new Channel[Any](this); rc }

  /**
   * Receives the next message from this actor's mailbox.
   */
  def ? : Any = receive {
    case x => x
  }

  def sender: OutputChannel[Any] = sessions.head

  def receiver: Actor = this

  private var continuation: PartialFunction[Any, Unit] = null
  private var onTimeout: Option[TimerTask] = None
  // accessed in Reaction
  private[actors] var isDetached = false
  private var isWaiting = false

  // guarded by lock of this
  protected def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) =
    if ((f eq null) && (continuation eq null)) {
      // do nothing (timeout is handled instead)
    }
    else {
      val task = new Reaction(this,
                              if (f eq null) continuation else f,
                              msg)
      scheduler execute task
    }

  private def tick(): Unit =
    scheduler tick this

  private[actors] var kill: () => Unit = () => {}

  private def suspendActor() {
    isWaiting = true
    while (isWaiting) {
      try {
        wait()
      } catch {
        case _: InterruptedException =>
      }
    }
    // links: check if we should exit
    if (shouldExit) exit()
  }

  private def suspendActorFor(msec: Long) {
    val ts = Platform.currentTime
    var waittime = msec
    var fromExc = false
    isWaiting = true
    while (isWaiting) {
      try {
        fromExc = false
        wait(waittime)
      } catch {
        case _: InterruptedException => {
          fromExc = true
          val now = Platform.currentTime
          val waited = now-ts
          waittime = msec-waited
          if (waittime < 0) { isWaiting = false }
        }
      }
      if (!fromExc) { isWaiting = false }
    }
    // links: check if we should exit
    if (shouldExit) exit()
  }

  private def resumeActor() {
    isWaiting = false
    notify()
  }

  /**
   * Starts this actor.
   */
  def start(): Actor = synchronized {
    // Reset various flags.
    //
    // Note that we do *not* reset `trapExit`. The reason is that
    // users should be able to set the field in the constructor
    // and before `act` is called.

    exitReason = 'normal
    exiting = false
    shouldExit = false

    scheduler execute {
      ActorGC.newActor(Actor.this)
      (new Reaction(Actor.this)).run()
    }

    this
  }

  private def seq[a, b](first: => a, next: => b): Unit = {
    val s = Actor.self
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

  private[actors] var links: List[AbstractActor] = Nil

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param to ...
   * @return   ...
   */
  def link(to: AbstractActor): AbstractActor = {
    assert(Actor.self == this, "link called on actor different from self")
    synchronized {
      links = to :: links
    }
    to.linkTo(this)
    to
  }

  /**
   * Links <code>self</code> to actor defined by <code>body</code>.
   */
  def link(body: => Unit): Actor = {
    val actor = new Actor {
      def act() = body
    }
    link(actor)
    actor.start()
    actor
  }

  private[actors] def linkTo(to: AbstractActor) = synchronized {
    links = to :: links
  }

  /**
   * Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: AbstractActor) {
    assert(Actor.self == this, "unlink called on actor different from self")
    synchronized {
      links = links.remove(from.==)
    }
    from.unlinkFrom(this)
  }

  private[actors] def unlinkFrom(from: AbstractActor) = synchronized {
    links = links.remove(from.==)
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
  def exit(reason: AnyRef): Nothing = {
    exitReason = reason
    exit()
  }

  /**
   * Terminates with exit reason <code>'normal</code>.
   */
  def exit(): Nothing = {
    // links
    if (!links.isEmpty)
      exitLinked()
    throw new ExitActorException
  }

  // Assume !links.isEmpty
  private[actors] def exitLinked() {
    exiting = true
    // remove this from links
    val mylinks = links.remove(this.==)
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
      this.synchronized {
        shouldExit = true
        exitReason = reason
        if (isSuspended)
          resumeActor()
        else if (isDetached)
          scheduleActor(null, null)
      }
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
private[actors] class SuspendActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = this
}
