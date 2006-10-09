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
 * @author Philipp Haller
 */
trait Reactor extends Actor {
  private var lastSender: Actor = null
  private[actors] def sender: Actor = lastSender
  private[actors] def pushSender(sender: Actor): Unit = lastSender = sender
  private[actors] def popSender(): Unit = lastSender = null

  private[actors] def isThreaded = false

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

  private[actors] def defaultDetachActor: PartialFunction[Any, Unit] => Unit =
    (f: PartialFunction[Any, Unit]) => {
      continuation = f
      throw new SuspendActorException
    }

  private[actors] def resetActor(): Unit = {
    detachActor = defaultDetachActor
    suspendActor = () => error("suspendActor called on reactor.")
    suspendActorFor = (msec: long) => error("suspendActorFor called on reactor.")
    kill = () => {}
  }

  resetActor()

  /**
   * Starts this reactor.
   */
  def start(): Unit =
    Scheduler.execute(new StartTask(this))

  /**
   * Terminates this reactor, thereby influencing linked actors
   * (see Actor.exit).
   */
  def exit(reason: String): Unit = {
    exitReason = reason
    Thread.currentThread().interrupt()
  }
}

/**
 * The abstract class <code>Reaction</code> associates an instance
 * of a <code>Reactor</code> with a
 * <code>java.lang.Runnable</code>. It is also the super class of
 * the different kinds of tasks used for the execution of
 * <code>Reactor</code>s.
 *
 * @author Philipp Haller
 */
private[actors] abstract class Reaction extends Runnable {
  def actor: Reactor
}

/**
 * This class represents task items used to start the execution
 * of <code>Reactor</code>s.
 *
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
