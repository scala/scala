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
import scala.actors.scheduler.TerminationMonitor

object ActorGC extends TerminationMonitor {

  private[actors] def getPendingCount = synchronized {
    activeActors
  }

  private[actors] def setPendingCount(cnt: Int) = synchronized {
    activeActors = cnt
  }

}
