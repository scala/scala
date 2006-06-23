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
class MailBox {
  type Message = AnyRef
  case class TIMEOUT() extends Message

  /** Unconsumed messages. */
  var sent = new Queue[Message]

  var continuation: PartialFunction[Message,Unit] = null
  // more complex continuation
  var contCases: PartialFunction[Message,Message] = null
  var contThen: Message => unit = null

  def hasCont =
    if ((continuation == null) && (contCases == null)) false
    else true

  def contDefinedAt(msg: Message) =
    if (((continuation != null) && continuation.isDefinedAt(msg)) ||
        ((contCases != null) && contCases.isDefinedAt(msg)))
      true
    else
      false

  var isAlive = true
  var scheduled = false

  private var pendingSignal = false

  def send(msg: Message): unit = synchronized {
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
            if (!contDefinedAt(TIMEOUT())) die()
            else {
              val task = new ReceiverTask(this, TIMEOUT())
              //Debug.info("ready to receive. dispatch new task " + task)
              scheduled = true
              Scheduler.execute(task)
            }
          case _ =>
            if (!contDefinedAt(msg))
              sent += msg
            else {
              if (pendingSignal) {
                pendingSignal = false
                TimerThread.trashRequest(this)
              }
              val task = new ReceiverTask(this, msg)
              //Debug.info("ready to receive. dispatch new task " + task)
              scheduled = true
              Scheduler.execute(task)
            }
        }
    }
  }

  def receiveMsg(msg: MailBox#Message) = {
    //Debug.info("" + Thread.currentThread() + ": Resuming " + this)
    if (continuation != null) {
      val f = continuation
      continuation = null
      scheduled = false
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

  def receive(f: PartialFunction[Message,unit]): scala.All = {
    if (isAlive) {
      Scheduler.tick(this)
      continuation = null
      sent.dequeueFirst(f.isDefinedAt) match {
        case Some(msg) =>
	  f(msg)
          die()
        case None =>
          continuation = f
          //Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
      }
    }
    throw new Done
  }

  def receiveWithin(msec: long)(f: PartialFunction[Message, unit]): scala.All = {
    Scheduler.tick(this)
    continuation = null
    sent.dequeueFirst(f.isDefinedAt) match {
      case Some(msg) =>
	f(msg)
        die()
      case None =>
        // if timeout == 0 then execute timeout action if specified (see Erlang book)
        if (msec == 0) {
          if (f.isDefinedAt(TIMEOUT()))
            f(TIMEOUT())
          die()
        }
        else {
          if (msec > 0) {
            TimerThread.requestTimeout(this, msec)
            pendingSignal = true
          }
          continuation = f
          //Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
        }
    }
    throw new Done
  }

  // original wish:
  // receiveAndReturn[A, B](cases: PartialFunction[Message, A], then: A => B): B
  // receiveAndReturn[A](cases: PartialFunction[Message, A], then: A => unit): unit

  def receiveAndReturn(cases: PartialFunction[Message,Message], then: Message => unit): unit = {
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

  class ReceiveAndReturn(cases: PartialFunction[Message,Message]) {
    def then(body: Message => unit): unit = receiveAndReturn(cases, body)
  }

  def receiv(cases: PartialFunction[Message,Message]): ReceiveAndReturn =
    new ReceiveAndReturn(cases)

  def die() = {
    if (isAlive) {
      isAlive = false
      //Debug.info("" + this + " died.")
    }
  }
}
