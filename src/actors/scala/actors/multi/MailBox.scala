/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.multi

import scala.collection.mutable.Queue

/**
 * @author Philipp Haller
 */
trait MailBox {
  /** Unconsumed messages. */
  var sent = new Queue[Any]

  var continuation: PartialFunction[Any, Unit] = null
  // more complex continuation
  var contCases: PartialFunction[Any, Any] = null
  var contThen: Any => Unit = null

  def hasCont =
    if ((continuation == null) && (contCases == null)) false
    else true

  def contDefinedAt(msg: Any) =
    if (((continuation != null) && continuation.isDefinedAt(msg)) ||
        ((contCases != null) && contCases.isDefinedAt(msg)))
      true
    else
      false

  var isAlive = true
  var scheduled = false

  private var pendingSignal = false

  def scheduleContinuation(msg: Any): Unit = {
    val task = new ReceiverTask(this, msg)
    //Debug.info("ready to receive. dispatch new task " + task)
    scheduled = true
    Scheduler.execute(task)
  }

  def send(msg: Any): Unit = synchronized {
    if (isAlive) {
      if (!hasCont || scheduled) {
        //Debug.info("no cont avail/task already scheduled. appending msg to mailbox.")
        msg match {
          case Signal() =>
            // do not add to mailbox
          case _ =>
            sent += msg
        }
      }
      else
        msg match {
          case Signal() =>
            if (!contDefinedAt(TIMEOUT)) die()
            else
              scheduleContinuation(TIMEOUT)
          case _ =>
            if (!contDefinedAt(msg))
              sent += msg
            else {
              if (pendingSignal) {
                pendingSignal = false
                TimerThread.trashRequest(this)
              }
              scheduleContinuation(msg)
            }
        }
    }
  }

  def receiveMsg(msg: Any) = {
    //Debug.info("" + Thread.currentThread() + ": Resuming " + this)
    if (continuation != null) {
      val f = continuation
      this.synchronized {
        continuation = null
        scheduled = false
      }
      f(msg)
      die()
    }
    else {
      // use more complex receive-and-return continuation
      val cases = contCases
      val then = contThen
      contCases = null
      contThen = null
      scheduled = false
      val result = cases(msg)
      then(result)
      die()
    }
  }

  def receive(f: PartialFunction[Any, Unit]): Nothing = synchronized {
    if (isAlive) {
      Scheduler.tick(this)
      continuation = null
      sent.dequeueFirst(f.isDefinedAt) match {
        case Some(msg) =>
          continuation = f
          scheduleContinuation(msg)
        case None =>
          continuation = f
          //Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
      }
    }
    throw new Done
  }

  def receiveWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = synchronized {
    if (isAlive) {
      Scheduler.tick(this)
      continuation = null
      sent.dequeueFirst(f.isDefinedAt) match {
        case Some(msg) => {
          continuation = f
          scheduleContinuation(msg)
        }
        case None =>
          // if timeout == 0 then execute timeout action if specified (see Erlang book)
          if (msec == 0) {
            if (f.isDefinedAt(TIMEOUT)) {
              continuation = f
              scheduleContinuation(TIMEOUT)
            }
            die()
          } else {
            if (msec > 0) {
              TimerThread.requestTimeout(this, msec)
              pendingSignal = true
            }
            continuation = f
            //Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
          }
      }
    }
    throw new Done
  }

  def receiveAndReturn(cases: PartialFunction[Any, Any], then: Any => Unit): Unit = {
    contCases = null
    contThen = null
    sent.dequeueFirst(cases.isDefinedAt) match {
      case Some(msg) => {
        val result = cases(msg)
        then(result)
        die()
      }
      case None => {
        contCases = cases
        contThen = then
        //Debug.info("No msg found. Saved complex continuation.")
      }
    }
    throw new Done
  }

  // receiv {...} then (msg => {...msg...})

  class ReceiveAndReturn(cases: PartialFunction[Any, Any]) {
    def then(body: Any => Unit): Unit = receiveAndReturn(cases, body)
  }

  def receiv(cases: PartialFunction[Any, Any]): ReceiveAndReturn =
    new ReceiveAndReturn(cases)

  def die() = if (isAlive) {
    isAlive = false
    //Debug.info("" + this + " died.")
  }
}
