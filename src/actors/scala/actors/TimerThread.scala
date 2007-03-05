/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import compat.Platform
import java.lang.{InterruptedException, Runnable, Thread}
import scala.collection.mutable.PriorityQueue

/**
 * This class allows the (local) sending of a message to an actor after
 * a timeout.  Used by the library to build <code>receiveWithin(time: long)</code>.
 * Note that the library deletes non-received <code>TIMEOUT</code> message if a
 * message is received before the time-out occurs.
 *
 * @version 0.9.4
 * @author Sebastien Noir, Philipp Haller
 */

object TimerThread extends AnyRef with Runnable {

  case class WakedActor(actor: Actor, f: PartialFunction[Any, Unit], time: long)
       extends Ordered[WakedActor] {
    var valid = true
    def compare(that: WakedActor): int = -(this.time compare that.time)
  }

  var queue = new PriorityQueue[WakedActor]
  val t = new Thread(this); t.start

  var lateList: List[WakedActor] = Nil

  /**
   * @param a ...
   */
  def trashRequest(a: Actor) = synchronized {
    // keep in mind: killing dead people is a bad idea!
    queue.elements.find((wa: WakedActor) => wa.actor == a && wa.valid) match {
      case Some(b) =>
        b.valid = false
      case None =>
        lateList.find((wa2: WakedActor) => wa2.actor == a && wa2.valid) match {
          case Some(b2) =>
            b2.valid = false
          case None =>
        }
    }
  }

  override def run = {
    try {
      while(true) {
        this.synchronized {
          try {
            val sleepTime = dequeueLateAndGetSleepTime
            if (lateList.isEmpty) wait(sleepTime)
          } catch {
            case t: Throwable => { throw t }
          }
        }

        // process guys waiting for signal and empty list
        for (val wa <- lateList) {
          if (wa.valid) {
            wa.actor ! TIMEOUT
          }
        }
        lateList = Nil
      }
    } catch {
      case consumed: InterruptedException =>
        // allow thread to quit
    }
  }

  /**
   * @param a          ...
   * @param f          ...
   * @param waitMillis ...
   */
  def requestTimeout(a: Actor, f: PartialFunction[Any, Unit], waitMillis: long): unit = synchronized {
    val wakeTime = now + waitMillis
    if (waitMillis <= 0) {
      a ! TIMEOUT
      return
    }

    if (queue.isEmpty) { // add to queue and restart sleeping
      queue += WakedActor(a, f, wakeTime)
      notify()
    } else
      if (queue.max.time > wakeTime) { // add to 1st position and restart sleeping
        queue += WakedActor (a, f, wakeTime)
        notify()
      }
      else // simply add to queue
        queue += WakedActor (a, f, wakeTime)
  }

  private def dequeueLateAndGetSleepTime: long = {
    val FOREVER: long = 0
    var waitingList: List[WakedActor] = Nil

    while (!queue.isEmpty) {
      val next = queue.max.time
      val amount = next - now
      if (amount > 0) { // guy in queue is not late
        lateList = waitingList // give back the list of waiting guys for signaling
        return amount
      }
      else // we're late: dequeue and examine next guy
        waitingList = queue.dequeue :: waitingList
    }

    // empty queue => sleep forever
    lateList = waitingList
    return FOREVER
  }

  def now = Platform.currentTime
}
