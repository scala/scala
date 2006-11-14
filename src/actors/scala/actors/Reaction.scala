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
 * The abstract class <code>Reaction</code> associates
 * an instance of an <code>Actor</code> with a
 * <code>java.lang.Runnable</code>.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
private[actors] class Reaction(a: Actor,
                               f: PartialFunction[Any, Unit],
                               msg: Any) extends Runnable {
  def this(a: Actor) = this(a, null, null)

  def actor = a

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

  def run(): Unit = {
    val t = currentThread
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    Scheduler.unPendReaction
    try {
      if (f == null)
        a.act()
      else
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
