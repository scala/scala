/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:$

package scala.actors

import scala.collection.mutable.HashMap

trait TerminationMonitor extends IScheduler {

  private var pendingReactions = 0
  private val termHandlers = new HashMap[OutputChannelActor, () => Unit]

  /** newActor is invoked whenever a new actor is started. */
  def newActor(a: OutputChannelActor) = synchronized {
    pendingReactions += 1
  }

  /** Registers a closure to be executed when the specified
   *  actor terminates.
   *
   *  @param  a  the actor
   *  @param  f  the closure to be registered
   */
  def onTerminate(a: OutputChannelActor)(f: => Unit) = synchronized {
    termHandlers += (a -> (() => f))
  }

  def terminated(a: OutputChannelActor) = synchronized {
    // obtain termination handler (if any)
    val todo = synchronized {
      termHandlers.get(a) match {
        case Some(handler) =>
          termHandlers -= a
          () => handler
        case None =>
          () => { /* do nothing */ }
      }
    }

    // invoke termination handler (if any)
    todo()

    synchronized {
      pendingReactions -= 1
    }
  }

  protected def allTerminated: Boolean = synchronized {
    pendingReactions <= 0
  }

}
