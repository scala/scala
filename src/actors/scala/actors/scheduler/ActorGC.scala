/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package scheduler

import java.lang.ref.{Reference, WeakReference, ReferenceQueue}
import scala.collection.mutable

/**
 * ActorGC keeps track of the number of live actors being managed by a
 * a scheduler so that it can shutdown when all of the actors it manages have
 * either been explicitly terminated or garbage collected.
 *
 * When an actor is started, it is registered with the ActorGC via the
 * `newActor` method, and when an actor is knowingly terminated
 * (e.g. act method finishes, exit explicitly called, an exception is thrown),
 * the ActorGC is informed via the `terminated` method.
 */
trait ActorGC extends TerminationMonitor {
  self: IScheduler =>

  /** Actors are added to refQ in newActor. */
  private val refQ = new ReferenceQueue[TrackedReactor]

  /**
   * This is a set of references to all the actors registered with
   * this ActorGC. It is maintained so that the WeakReferences will
   * not be GC'd before the actors to which they point.
   */
  private val refSet = new mutable.HashSet[Reference[t] forSome { type t <: TrackedReactor }]

  /** newActor is invoked whenever a new actor is started. */
  override def newActor(a: TrackedReactor) = synchronized {
    // registers a reference to the actor with the ReferenceQueue
    val wr = new WeakReference[TrackedReactor](a, refQ)
    refSet += wr
    activeActors += 1
  }

  /** Checks for actors that have become garbage. */
  protected override def gc() = synchronized {
    // check for unreachable actors
    def drainRefQ() {
      val wr = refQ.poll
      if (wr != null) {
        activeActors -= 1
        refSet -= wr
        // continue draining
        drainRefQ()
      }
    }
    drainRefQ()
  }

  /** Prints some status information on currently managed actors. */
  protected def status() {
    println(this+": size of refSet: "+refSet.size)
  }

  /** Checks whether all actors have terminated. */
  override private[actors] def allActorsTerminated: Boolean = synchronized {
    activeActors <= 0
  }

  override def onTerminate(a: TrackedReactor)(f: => Unit): Unit = synchronized {
    terminationHandlers += (a -> (() => f))
  }

  override def terminated(a: TrackedReactor) = {
    super.terminated(a)

    synchronized {
      // find the weak reference that points to the terminated actor, if any
      refSet.find((ref: Reference[t] forSome { type t <: TrackedReactor }) => ref.get() == a) match {
        case Some(r) =>
          // invoking clear will not cause r to be enqueued
          r.clear()
          refSet -= r.asInstanceOf[Reference[t] forSome { type t <: TrackedReactor }]
        case None =>
          // do nothing
      }
    }
  }

  private[actors] def getPendingCount = synchronized {
    activeActors
  }

  private[actors] def setPendingCount(cnt: Int) = synchronized {
    activeActors = cnt
  }

}
