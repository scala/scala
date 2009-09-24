/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.lang.ref.{Reference, WeakReference, ReferenceQueue}

import scala.collection.mutable.{HashMap, HashSet}
import scala.actors.scheduler.TerminationMonitor

/**
 * ActorGC keeps track of the number of live actors being managed by a
 * a scheduler so that it can shutdown when all of the actors it manages have
 * either been explicitly terminated or garbage collected.
 *
 * When an actor is started, it is registered with the ActorGC via the
 * <code>newActor</code> method, and when an actor is knowingly terminated
 * (e.g. act method finishes, exit explicitly called, an exception is thrown),
 * the ActorGC is informed via the <code>terminated</code> method.
 */
trait ActorGC extends TerminationMonitor {
  self: IScheduler =>

  /** Actors are added to refQ in newActor. */
  private val refQ = new ReferenceQueue[Reactor]

  /**
   * This is a set of references to all the actors registered with
   * this ActorGC. It is maintained so that the WeakReferences will not be GC'd
   * before the actors to which they point.
   */
  private val refSet = new HashSet[Reference[t] forSome { type t <: Reactor }]

  /** newActor is invoked whenever a new actor is started. */
  override def newActor(a: Reactor) = synchronized {
    // registers a reference to the actor with the ReferenceQueue
    val wr = new WeakReference[Reactor](a, refQ)
    refSet += wr
    pendingReactions += 1
  }

  /** Removes unreachable actors from refSet. */
  protected override def gc() = synchronized {
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

  protected def status() {
    println(this+": size of refSet: "+refSet.size)
  }

  protected override def allTerminated: Boolean = synchronized {
    pendingReactions <= 0
  }

  override def onTerminate(a: Reactor)(f: => Unit): Unit = synchronized {
    termHandlers += (a -> (() => f))
  }

  /* Called only from <code>Reaction</code>.
   */
  override def terminated(a: Reactor) = synchronized {
    // execute registered termination handler (if any)
    termHandlers.get(a) match {
      case Some(handler) =>
        handler()
        // remove mapping
        termHandlers -= a
      case None =>
        // do nothing
    }

    // find the weak reference that points to the terminated actor, if any
    refSet.find((ref: Reference[t] forSome { type t <: Reactor }) => ref.get() == a) match {
      case Some(r) =>
        // invoking clear will not cause r to be enqueued
        r.clear()
        refSet -= r.asInstanceOf[Reference[t] forSome { type t <: Reactor }]
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
