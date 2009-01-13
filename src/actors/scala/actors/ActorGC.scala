/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:$

package scala.actors

import java.lang.ref.{Reference, WeakReference, ReferenceQueue}
import java.util.WeakHashMap

import scala.collection.mutable.{HashMap, HashSet}

object ActorGC {

  private var pendingReactions = 0
  private val termHandlers = new HashMap[Actor, () => Unit]

  private val refQ = new ReferenceQueue[Actor]
  private val refSet = new HashSet[Reference[t] forSome { type t <: Actor }]

  def newActor(a: Actor) = synchronized {
    val wr = new WeakReference[Actor](a, refQ)
    refSet += wr
    pendingReactions += 1
  }

  def gc() = synchronized {
    // check for unreachable actors
    def drainRefQ() {
      val wr = refQ.poll
      if (wr != null) {
        pendingReactions -= 1
        refSet -= wr
        // continue draining
        drainRefQ()
      }
    }
    drainRefQ()
  }

  def status() {
    println("ActorGC: size of refSet: "+refSet.size)
  }

  def allTerminated: Boolean = synchronized {
    pendingReactions <= 0
  }

  private[actors] def onTerminate(a: Actor)(f: => Unit) = synchronized {
    termHandlers += (a -> (() => f))
  }

  /* Called only from <code>Reaction</code>.
   */
  private[actors] def terminated(a: Actor) = synchronized {
    // execute registered termination handler (if any)
    termHandlers.get(a) match {
      case Some(handler) =>
        handler()
        // remove mapping
        termHandlers -= a
      case None =>
        // do nothing
    }

    pendingReactions -= 1
  }

  private[actors] def getPendingCount = synchronized {
    pendingReactions
  }

  private[actors] def setPendingCount(cnt: Int) = synchronized {
    pendingReactions = cnt
  }

}
