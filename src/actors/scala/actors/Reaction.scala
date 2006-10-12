/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.actors

/**
 * The abstract class <code>Reaction</code> associates an instance
 * of an <code>Actor</code> with a
 * <code>java.lang.Runnable</code>. It is also the super class of
 * the different kinds of tasks used for the execution of
 * event-based <code>Actor</code>s.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] abstract class Reaction extends Runnable {
  def actor: Actor
}

/**
 * This class represents task items used to start the execution
 * of <code>Actor</code>s.
 *
 * @version Beta2
 * @author Philipp Haller
 */
private[actors] class StartTask(a: Actor) extends Reaction {
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
private[actors] class ActorTask(a: Actor,
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
