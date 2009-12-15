/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors
package scheduler

import scala.collection.mutable.HashMap

trait TerminationMonitor {

  protected var activeActors = 0
  protected val terminationHandlers = new HashMap[Reactor, () => Unit]
  private var started = false

  /** newActor is invoked whenever a new actor is started. */
  def newActor(a: Reactor) = synchronized {
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
  def onTerminate(a: Reactor)(f: => Unit): Unit = synchronized {
    terminationHandlers += (a -> (() => f))
  }

  /** Registers that the specified actor has terminated.
   *
   *  @param  a  the actor that has terminated
   */
  def terminated(a: Reactor) = {
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

  /** Deprecated non-actor-private version */
  @deprecated("this method is going to be removed in a future release")
  def allTerminated: Boolean = allActorsTerminated

  /** Checks for actors that have become garbage. */
  protected def gc() {}
}
