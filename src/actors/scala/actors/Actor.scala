/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.{HashSet, Queue}
import scala.compat.Platform

/**
 * The <code>Actor</code> object provides functions for the definition of
 * actors, as well as all actor operations, such as
 * <code>receive</code>, <code>react</code>, <code>reply</code>,
 * etc.
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
object Actor {

  private[actors] val tl = new ThreadLocal

  /**
   * Returns the currently executing actor. Should be used instead
   * of <code>this</code> in all blocks of code executed by
   * actors.
   *
   * @return returns the currently executing actor.
   */
  def self: Actor = synchronized {
    var a = tl.get.asInstanceOf[Actor]
    if (null eq a) {
      a = new ActorProxy(currentThread)
      tl.set(a)
    }
    a
  }

  /**
   * <p>This function is used for the definition of actors.</p>
   * <p>The following example demonstrates its usage:</p><pre>
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
  def actor(body: => Unit): Actor = synchronized {
    val actor = new Actor {
      def act() = body
    }
    actor.start()
    actor
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
  def receive[a](f: PartialFunction[Any, a]): a =
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
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R =
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
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing =
    self.reactWithin(msec)(f)

  def eventloop(f: PartialFunction[Any, Unit]): Nothing =
    self.react(new RecursiveProxyHandler(self, f))

  private class RecursiveProxyHandler(a: Actor, f: PartialFunction[Any, Unit])
          extends PartialFunction[Any, Unit] {
    def isDefinedAt(m: Any): boolean =
      true // events are immediately removed from the mailbox
    def apply(m: Any): Unit = {
      if (f.isDefinedAt(m)) f(m)
      self.react(this)
    }
  }

  /**
   * Returns the actor which sent the last received message.
   */
  def sender: Actor = self.sender

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

  private[actors] trait Body[a] {
    def andThen[b](other: => b): Nothing
  }

  implicit def mkBody[a](body: => a) = new Body[a] {
    def andThen[b](other: => b): Nothing = seq(body, other)
  }

  /**
   * Causes <code>self</code> to repeatedly execute
   * <code>body</code>.
   *
   * @param body the code block to be executed
   */
  def loop(body: => Unit): Nothing = body andThen loop(body)

  /**
   * Causes <code>self</code> to execute <code>first</code>
   * followed by <code>next</code>.
   *
   * @param first the first code block to be executed
   * @param next  the second code block to be executed
   */
  def seq[a, b](first: => a, next: => b): Nothing = {
    val s = self
    val killNext = s.kill
    s.kill = () => { s.kill = killNext; next; exit('normal) }
    first
    exit('normal)
  }

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param  to the actor to link to
   * @return
   */
  def link(to: Actor): Actor = self.link(to)

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
   *   <code>{'EXIT, self, reason}</code> to <code>a</code>.
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>false</code> (default),
   *   call <code>a.exit(reason)</code> if
   *   <code>reason != 'normal</code>.
   * </p>
   */
  def exit(reason: AnyRef): Nothing = self.exit(reason)
}

/**
 * <p>
 *   This class provides (together with <code>Channel</code>) an
 *   implementation of event-based actors.
 * </p>
 * <p>
 *   The main ideas of our approach are explained in the papers<br>
 *   <b>Event-Based Programming without Inversion of Control</b>,
 *   Philipp Haller and Martin Odersky, <i>Proc. JMLC 2006</i>
 *   <br><br>
 *   <b>Actors that Unify Threads and Events</b>,
 *   Philipp Haller and Martin Odersky, <i>LAMP-REPORT-2007-001, EPFL</i>
 * </p>
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
trait Actor extends OutputChannel[Any] {

  private var received: Option[Any] = None

  private[actors] val waitingForNone = (m: Any) => false
  private[actors] var waitingFor: Any => boolean = waitingForNone
  private[actors] var isSuspended = false

  private val mailbox = new MessageQueue
  private var sessions: List[Channel[Any]] = Nil

  private def send(msg: Any, session: Channel[Any]) = synchronized {
    tick()
    if (waitingFor(msg)) {
      received = Some(msg)
      sessions = session :: sessions
      waitingFor = waitingForNone

      if (timeoutPending) {
        timeoutPending = false
        TimerThread.trashRequest(this)
      }

      if (isSuspended)
        resumeActor()
      else
        scheduleActor(null, msg)
    } else {
      mailbox.append(msg, session)
    }
  }

  def receive[R](f: PartialFunction[Any, R]): R = {
    assert(Actor.self == this, "receive from channel belonging to other actor")
    if (shouldExit) exit() // links
    this.synchronized {
      tick()
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

  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self == this, "receive from channel belonging to other actor")
    if (shouldExit) exit() // links
    this.synchronized {
      tick()
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

  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == this, "react on channel belonging to other actor")
    if (shouldExit) exit() // links
    Scheduler.pendReaction
    this.synchronized {
      tick()
      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        waitingFor = f.isDefinedAt
        continuation = f
        isDetached = true
      } else {
        sessions = qel.session :: sessions
        scheduleActor(f, qel.msg)
      }
      throw new SuspendActorException
    }
  }

  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == this, "react on channel belonging to other actor")
    if (shouldExit) exit() // links
    Scheduler.pendReaction
    this.synchronized {
      tick()
      // first, remove spurious TIMEOUT message from mailbox if any
      val spurious = mailbox.extractFirst((m: Any) => m == TIMEOUT)

      val qel = mailbox.extractFirst((m: Any) => f.isDefinedAt(m))
      if (null eq qel) {
        waitingFor = f.isDefinedAt
        TimerThread.requestTimeout(this, f, msec)
        timeoutPending = true
        continuation = f
        isDetached = true
      } else {
        sessions = qel.session :: sessions
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
  def !(msg: Any): Unit = {
    send(msg, Actor.self.getReplyChannel)
  }

  /**
   * Forwards <code>msg</code> to this actor (asynchronous).
   */
  def forward(msg: Any): Unit = send(msg, Actor.sender.getReplyChannel)

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   */
  def !?(msg: Any): Any = {
    val replyChannel = Actor.self.freshReply()
    this ! msg
    replyChannel.receive {
      case x => x
    }
  }

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous) within <code>msec</code> milliseconds.
   * When the timeout occurs, <code>None</code> is returned.
   * Otherwise, returns <code>Some(value)</code> where
   * <code>value</code> is the reply value.
   */
  def !?(msec: long, msg: Any): Option[Any] = {
    val replyChannel = Actor.self.freshReply()
    this ! msg
    replyChannel.receiveWithin(msec) {
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
  def !![a](msg: Any, f: PartialFunction[Any, a]): Future[a] = {
    val ftch = new Channel[Any](Actor.self)
    send(msg, ftch)
    new Future[a](ftch) {
      def apply() =
        if (isSet) value.get
        else ch.receive {
          case any => value = Some(f(any)); value.get
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
   * Replies with <code>msg</code> to the sender waiting in
   * a synchronous send.
   */
  def reply(msg: Any): Unit = session ! msg

  private var rc = new Channel[Any](this)
  def getReplyChannel = rc
  def freshReply() = { rc = new Channel[Any]; rc }

  /**
   * Receives the next message from this actor's mailbox.
   */
  def ? : Any = receive {
    case x => x
  }

  private[actors] def sender: Actor =
    if (sessions.isEmpty) null
    else sessions.head.asInstanceOf[Channel[Any]].receiver

  private[actors] def session: Channel[Any] =
    if (sessions.isEmpty) null
    else sessions.head.asInstanceOf[Channel[Any]]

  private[actors] var continuation: PartialFunction[Any, Unit] = null
  private[actors] var timeoutPending = false
  private[actors] var isDetached = false
  private[actors] var isWaiting = false

  // guarded by lock of this
  private[actors] def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) =
    if ((f eq null) && (continuation eq null)) {
      // do nothing (timeout is handled instead)
    }
    else {
      val task = new Reaction(this,
                              if (f eq null) continuation else f,
                              msg)
      Scheduler execute task
    }

  private[actors] def tick(): Unit =
    Scheduler tick this

  private[actors] var kill = () => {}

  def suspendActor() {
    isWaiting = true
    while(isWaiting) {
      try {
        wait()
      } catch {
        case _: InterruptedException =>
      }
    }
    // links: check if we should exit
    if (shouldExit) exit()
  }

  def suspendActorFor(msec: long) {
    val ts = Platform.currentTime
    var waittime = msec
    var fromExc = false
    isWaiting = true
    while(isWaiting) {
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

  def resumeActor() {
    isWaiting = false
    notify()
  }

  /**
   * Starts this actor.
   */
  def start() {
    Scheduler start new Reaction(this)
  }

  private[actors] var links: List[Actor] = Nil

  /**
   * Links <code>self</code> to actor <code>to</code>.
   *
   * @param to ...
   * @return   ...
   */
  def link(to: Actor): Actor = {
    links = to :: links
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

  private[actors] def linkTo(to: Actor) {
    links = to :: links
  }

  /**
   Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: Actor) {
    links = links.remove(from.==)
    from.unlinkFrom(this)
  }

  private[actors] def unlinkFrom(from: Actor) {
    links = links.remove(from.==)
  }

  var trapExit = false
  private[actors] var exitReason: AnyRef = 'normal
  private[actors] var exiting = false
  private[actors] var shouldExit = false

  /**
   * <p>
   *   Terminates execution of <code>self</code> with the following
   *   effect on linked actors:
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>true</code>, send message
   *   <code>{'EXIT, self, reason}</code> to <code>a</code>.
   * </p>
   * <p>
   *   For each linked actor <code>a</code> with
   *   <code>trapExit</code> set to <code>false</code> (default),
   *   call <code>a.exit(reason)</code> if
   *   <code>reason != 'normal</code>.
   * </p>
   */
  def exit(reason: AnyRef): Nothing = {
    if (reason == 'normal) kill()
    // links
    if (!links.isEmpty) {
      exitReason = reason
      exitLinked()
    }
    throw new ExitActorException
  }

  /**
   * Terminates with exit reason <code>'normal</code>.
   */
  def exit(): Nothing = exit('normal)

  // Assume !links.isEmpty
  private[actors] def exitLinked() {
    exiting = true
    // remove this from links
    links = links.remove(this.==)
    // exit linked processes
    links.foreach((linked: Actor) => {
      unlink(linked)
      if (!linked.exiting)
        linked.exit(this, exitReason)
    })
  }

  // Assume !links.isEmpty
  private[actors] def exitLinked(reason: AnyRef) {
    exitReason = reason
    exiting = true
    // remove this from links
    links = links.remove(this.==)
    // exit linked processes
    links.foreach((linked: Actor) => {
      unlink(linked)
      if (!linked.exiting)
        linked.exit(this, exitReason)
    })
  }

  // Assume !this.exiting
  private[actors] def exit(from: Actor, reason: AnyRef) {
    if (trapExit) {
      this ! Triple('EXIT, from, reason)
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
 *    <a href="Actor.html#receiveWithin(scala.Long)" target="contentFrame">
 *    <code>receiveWithin</code></a> and
 *    <a href="Actor.html#reactWithin(scala.Long)" target="contentFrame">
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
 *  @version 0.9.4
 *  @author Philipp Haller
 */
case object TIMEOUT


/** <p>
 *    This class is used to manage control flow of actor
 *    executions.
 *  </p>
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
private[actors] class SuspendActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = this
}
