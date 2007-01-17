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

class ExitActorException extends Throwable

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

  def run(): Unit = {
    val t = currentThread
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    Scheduler.unPendReaction
    a.isDetached = false
    try {
      try {
        if (f == null)
          a.act()
        else
          f(msg)
        a.exit("normal")
      } catch {
        case _: ExitActorException =>
          throw new InterruptedException
      }
    }
    catch {
      case ie: InterruptedException => {
        a.exitLinked()
      }
      case d: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        a.exitLinked()
      }
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }

  private var runnable = false

  def isRunnable = synchronized {
    runnable
  }

  def setRunnable(on: boolean) = synchronized {
    runnable = on
  }
}
