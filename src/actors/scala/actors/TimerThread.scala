/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.PriorityQueue

import scala.actors.multi.Actor
import scala.actors.multi.MailBox

/**
 * This class allows the (locl) sending of a message to an actor after
 * a timeout.  Used by the library to build receiveWithin(time:long).
 * Note that the library deletes non received TIMEOUT() message if a
 * messsage is received before the time-out occurs.
 *
 * @author Sebastien Noir
 */
case class Signal()

object TimerThread extends AnyRef with Runnable {
  case class WakedActor(actor: MailBox, time: long, reason: String) extends Ordered[WakedActor] {
    var valid = true
    def compare [b >: WakedActor <% Ordered[b]](that: b): int = that match {
      case that2: WakedActor => -(this.time compare that2.time)
      case _ => error("not comparable")
    }
  }

  var queue = new PriorityQueue[WakedActor]
  val t = new Thread(this); t.start

  var lateList: List[WakedActor] = Nil

  def trashRequest(a: MailBox) = synchronized {
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

  override def run = while (true) {
    this.synchronized {
      try {
	val sleepTime = dequeueLateAndGetSleepTime
	if (lateList.isEmpty) {
	  wait(sleepTime)
	}
      } catch {
	case t: Throwable => { t.printStackTrace(); throw t }
      }
    }

    // process guys waiting for signal and empty list
    for (val wa <- lateList) {
      if (wa.valid) {
        wa.actor send Signal()
      }
    }
    lateList = Nil
  }

  def requestSignal(a: Actor, waitMillis: long, reason: String): unit = this.synchronized {
    Console.println("TTTT Actor "+a+" requests Signal in "+waitMillis +" ms for :"+reason)
    val wakeTime = now + waitMillis
    if (waitMillis < 0) {
      a send Signal()
      return
    }

    if (queue.isEmpty) { // add to queue and restart sleeping
      queue += WakedActor(a, wakeTime, reason)
      notify()
    } else { //queue full
      if (queue.max.time > wakeTime) { // add to 1st position and restart sleeping
  	   queue += WakedActor (a, wakeTime, reason)
	     notify()
      } else { // simply add to queue

	     queue += WakedActor (a, wakeTime, reason)
      }
    }
  }

  def requestTimeout(a: MailBox, waitMillis: long): unit = synchronized {
    val wakeTime = now + waitMillis
    if (waitMillis < 0) {
      a send Signal()
      return
    }

    if (queue.isEmpty) { // add to queue and restart sleeping
      queue += WakedActor(a, wakeTime, "")
      notify()
    } else
      if (queue.max.time > wakeTime) { // add to 1st position and restart sleeping
	      queue += WakedActor (a, wakeTime, "")
	      notify()
      }
      else // simply add to queue
	queue += WakedActor (a, wakeTime, "")
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

  def now = new java.util.Date().getTime()
}

//================================================================================

object TimerThreadTest {
  def main (args:Array[String]) = {
    new Tester (1000, "ONE").start
    new Tester (500, "TWO").start
  }

  class Tester (duration : int, name:String) extends Actor {
    var i = 0

    def loop:unit = {
      receive {
	case Signal() =>
	  Console.println(name + i)
	  i = i+1;
	  loop
      }
    }

    override def run = {
      for (val i <-List.range(1,10)) {
	TimerThread.requestSignal(this, (duration * i).asInstanceOf[long], ""+duration*i)
      }
      loop
    }
  }
}
