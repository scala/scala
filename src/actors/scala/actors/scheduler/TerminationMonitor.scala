/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package scheduler

import scala.collection.mutable

private[scheduler] trait TerminationMonitor {
  _: IScheduler =>

  protected var activeActors = 0
  protected val terminationHandlers = new mutable.HashMap[TrackedReactor, () => Unit]
  private var started = false

  /** newActor is invoked whenever a new actor is started. */
  def newActor(a: TrackedReactor) = synchronized {
    activeActors += 1
    if (!started)
      started = true
  }

  /** Registers a closure to be executed when the specified
   *  actor terminates.
   *
   *  @param  a  the actor
   *  @param  f  the closure to be registered
   */
  def onTerminate(a: TrackedReactor)(f: => Unit): Unit = synchronized {
    terminationHandlers += (a -> (() => f))
  }

  /** Registers that the specified actor has terminated.
   *
   *  @param  a  the actor that has terminated
   */
  def terminated(a: TrackedReactor) = {
    // obtain termination handler (if any)
    val todo = synchronized {
      terminationHandlers.get(a) match {
        case Some(handler) =>
          terminationHandlers -= a
          handler
        case None =>
          () => { /* do nothing */ }
      }
    }

    // invoke termination handler (if any)
    todo()

    synchronized {
      activeActors -= 1
    }
  }

  /** Checks whether all actors have terminated. */
  private[actors] def allActorsTerminated: Boolean = synchronized {
    started && activeActors <= 0
  }

  /** Checks for actors that have become garbage. */
  protected def gc() {}
}
