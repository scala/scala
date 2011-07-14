/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteGC.scala 17547 2009-04-21 13:56:28Z michelou $

package scala.runtime.remoting

import java.lang.ref.{Reference, WeakReference, ReferenceQueue}
import java.rmi.{NoSuchObjectException, Remote}
import java.rmi.server.UnicastRemoteObject
import scala.collection.mutable

/**
 *
 *  @author Stephane Micheloud
 *  @version 1.0
 */
// Adapted from scala.actors.ActorGC
private [runtime] class RemoteGC {

  private val refQueue = new ReferenceQueue[Remote]
  private val refSet = new mutable.HashSet[Reference[T] forSome { type T <: Remote }]

  private var liveRefs = 0

  def newRef(a: Remote) = synchronized {
    refSet += new WeakReference(a, refQueue)
    liveRefs += 1
    info("added object reference \""+a+"\" ("+liveRefs+")")
  }

  def gc() = synchronized {
    info("GC called ("+liveRefs+")")
    // check for unreachable object references
    def drain() {
      val wr = refQueue.poll
      if (wr != null) {
        val msg = try {
          UnicastRemoteObject.unexportObject(wr.get, true/*force*/)
          "removed object reference"
        }
        catch {
          case e: NoSuchObjectException =>
            "object already unbound"
        }
        info(msg+" ("+liveRefs+")")
        liveRefs -= 1
        refSet -= wr
        // continue draining
        drain()
      }
    }
    drain()
  }

  def allClosed: Boolean = synchronized {
    liveRefs <= 0
  }

  private def info(msg: String) { Debug.info("[RemoteGC] "+msg) }
}
