/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import java.lang.{InterruptedException, Runnable}
import java.util.logging.{Level, Logger}

/**
 * The abstract class <code>Reaction</code> associates an instance
 * of an <code>Actor</code> with a
 * <code>java.lang.Runnable</code>. It is also the super class of
 * the different kinds of tasks used for the execution of
 * event-based <code>Actor</code>s.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
private[actors] abstract class Reaction extends Runnable {
  def actor: Actor

  /**
   * @param t ...
   */
  def log(t: Throwable): unit = {
    Debug.info("logging "+t)
    val logger = Logger.getLogger("Scheduler")
    val buf = new StringBuffer
    buf.append("Exception caught by task:\n")
    buf.append(t.toString()+"\n")
    val trace = t.getStackTrace()
    for (val elem <- trace) {
      buf.append(elem.toString() + "\n")
    }
    logger.log(Level.FINE, buf.toString())
  }
}

/**
 * This class represents task items used to start the execution
 * of <code>Actor</code>s.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
private[actors] class StartTask(a: Actor) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = currentThread
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    Scheduler.unPendReaction
    try {
      a.act()
      if (currentThread.isInterrupted())
        throw new InterruptedException
      a.kill()
      if (currentThread.isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case ie: InterruptedException => {
        log(ie)
        a.exitLinked()
      }
      case d: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        log(t)
        a.exit(t.toString())
      }
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
 * @version 0.9.0
 * @author Philipp Haller
 */
private[actors] class ActorTask(a: Actor,
                                f: PartialFunction[Any, Unit],
                                msg: Any) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = currentThread
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    Scheduler.unPendReaction
    try {
      f(msg)
      if (currentThread.isInterrupted())
        throw new InterruptedException
      a.kill()
      if (currentThread.isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case ie: InterruptedException => {
        log(ie)
        a.exitLinked()
      }
      case d: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        log(t)
        a.exit(t.toString())
      }
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }
}
