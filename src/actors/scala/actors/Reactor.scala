/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * This class provides (together with <code>Channel</code>) an
 * implementation of event-based actors (aka reactors).
 *
 * The main ideas of our approach are explained in the paper<br>
 * <b>Event-Based Programming without Inversion of Control</b>, Philipp Haller, Martin Odersky <i>Proc. JMLC 2006</i>
 *
 * @version Beta2
 * @author Philipp Haller
 */
trait Reactor extends Actor {

  private[actors] val in = new Channel[Any]
  in.receiver = this

  private var rc: Channel[Any] = null

  private[actors] def reply: Channel[Any] = {
    if (rc == null) {
      rc = new Channel[Any]
      rc.receiver = this
    }
    rc
  }

  private[actors] def freshReply(): Unit = {
    rc = new Channel[Any]
    rc.receiver = this
  }

  /**
   * The behavior of an actor is specified by implementing this
   * abstract method. Note that the preferred way to create actors
   * is through the <code>actor</code> and <code>reactor</code>
   * methods defined in object <code>Actor</code>.
   */
  def act(): Unit

  /**
   * Sends <code>msg</code> to this actor (asynchronous).
   */
  def !(msg: Any): Unit = in ! msg

  def forward(msg: Any): Unit = in forward msg

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   */
  def !?(msg: Any): Any = in !? msg

  private val lastSenders = new scala.collection.mutable.Stack[Actor]

  private[actors] def sender: Actor = {
    if (lastSenders.isEmpty) null
    else lastSenders.top
  }

  private[actors] def pushSender(s: Actor) = { lastSenders.push(s) }
  private[actors] def popSender(): Unit = { lastSenders.pop }

  private[actors] var continuation: PartialFunction[Any, Unit] = null
  private[actors] var timeoutPending = false

  private[actors] def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) =
    if (f == null && continuation == null) {
      // do nothing (timeout is handled instead)
    }
    else {
      val task = new ActorTask(this,
                               if (f == null) continuation else f,
                               msg)
      Scheduler.execute(task)
    }

  private[actors] def tick(): Unit =
    Scheduler.tick(this)

  private[actors] def defaultDetachActor: PartialFunction[Any, Unit] => Unit =
    (f: PartialFunction[Any, Unit]) => {
      continuation = f
      throw new SuspendActorException
    }

  private[actors] var suspendActor: () => Unit = _
  private[actors] var suspendActorFor: long => Unit = _
  private[actors] var resumeActor: () => Unit = _
  private[actors] var detachActor: PartialFunction[Any, Unit] => Unit = _
  private[actors] var kill: () => Unit = _

  private[actors] def resetActor(): Unit = {
    suspendActor = () => wait()
    suspendActorFor = (msec: long) => wait(msec)
    resumeActor = () => notify()
    detachActor = defaultDetachActor
    kill = () => {}
  }

  resetActor()

  /**
   * Starts this reactor.
   */
  def start(): Unit =
    Scheduler.execute(new StartTask(this))

  private val links = new HashSet[Actor]

  /**
   Links <code>self</code> to actor <code>to</code>.
   */
  def link(to: Actor): Actor = {
    links += to
    to.linkTo(this)
    to
  }

  /**
   Links <code>self</code> to actor defined by <code>body</code>.
   */
  def link(body: => Unit): Actor = {
    val actor = new Reactor {
      def act() = body
    }
    link(actor)
    actor.start()
    actor
  }

  private[actors] def linkTo(to: Actor): Unit =
    links += to

  /**
   Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: Actor): Unit = {
    links -= from
    from.unlinkFrom(this)
  }

  private[actors] def unlinkFrom(from: Actor): Unit =
    links -= from

  var trapExit = false

  private[actors] var exitReason: String = ""

  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  def exit(reason: String): Unit = {
    exitReason = reason
    Thread.currentThread().interrupt()
  }

  private[actors] def exit(from: Actor, reason: String): Unit = {
    if (from == this) {
      exit(reason)
    }
    else {
      if (trapExit)
        this ! Exit(from, reason)
      else if (!reason.equals("normal"))
        exit(reason)
    }
  }

  private[actors] def exitLinked(): Unit =
    exitLinked(exitReason, new HashSet[Actor])

  private[actors] def exitLinked(reason: String): Unit =
    exitLinked(reason, new HashSet[Actor])

  private[actors] def exitLinked(reason: String,
                                 exitMarks: HashSet[Actor]): Unit = {
    if (exitMarks contains this) {
      // we are marked, do nothing
    }
    else {
      exitMarks += this // mark this as exiting
      // exit linked processes
      val iter = links.elements
      while (iter.hasNext) {
        val linked = iter.next
        unlink(linked)
        linked.exit(this, reason)
      }
      exitMarks -= this
    }
  }
}

/**
 * The abstract class <code>Reaction</code> associates an instance
 * of a <code>Reactor</code> with a
 * <code>java.lang.Runnable</code>. It is also the super class of
 * the different kinds of tasks used for the execution of
 * <code>Reactor</code>s.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] abstract class Reaction extends Runnable {
  def actor: Reactor
}

/**
 * This class represents task items used to start the execution
 * of <code>Reactor</code>s.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] class StartTask(a: Reactor) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = Thread.currentThread()
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    try {
      a.act()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.kill()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case _: InterruptedException =>
        a.exitLinked()
      case d: SuspendActorException =>
        // do nothing (continuation is already saved)
      case t: Throwable =>
        a.exit(t.toString())
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }
}

/**
 * This class represents task items used to execute actions
 * specified in arguments of <code>react</code> and
 * <code>reactWithin</code>.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] class ActorTask(a: Reactor,
                                f: PartialFunction[Any, Unit],
                                msg: Any) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = Thread.currentThread()
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    try {
      f(msg)
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.kill()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case _: InterruptedException =>
        a.exitLinked()
      case d: SuspendActorException =>
        // do nothing (continuation is already saved)
      case t: Throwable =>
        a.exit(t.toString())
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }
}
