/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import Actor._

case object TIMEOUT

class SuspendActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = {
    this
  }
}

/**
 * This class provides a means for typed communication among
 * actors. Only the actor creating an instance of a
 * <code>Channel</code> may receive from it.
 *
 * @version 0.9.0
 * @author Philipp Haller
 */
class Channel[Msg] extends InputChannel[Msg] with OutputChannel[Msg] {

  private[actors] var receiver: Actor = synchronized {
    // basically Actor.self, but can be null
    Actor.selfs.get(currentThread).asInstanceOf[Actor]
  }

  private var received: Option[Msg] = None

  private[actors] val waitingForNone = (m: Msg) => false
  private[actors] var waitingFor: Msg => boolean = waitingForNone
  private[actors] var waitingForSender: Actor = null

  private[actors] var isSuspended = false

  //private val messageQueue = new MessageQueue[Msg]
  private val mailbox = new scala.collection.mutable.Queue[Pair[Msg, Actor]]

  private def send(msg: Msg, sender: Actor) = receiver.synchronized {
    receiver.tick()
    if (waitingFor(msg) && ((waitingForSender eq null) ||
        (waitingForSender == sender))) {
      received = Some(msg)
      receiver.pushSender(sender)
      waitingFor = waitingForNone
      waitingForSender = null

      if (receiver.timeoutPending) {
        receiver.timeoutPending = false
        TimerThread.trashRequest(receiver)
      }

      if (isSuspended)
        receiver.resumeActor()
      else
        receiver.scheduleActor(null, msg)
    } else {
      //messageQueue.append(msg, sender)
      mailbox += Pair(msg, sender)
    }
  }

  /**
   * Sends <code>msg</code> to this <code>Channel</code>.
   */
  def !(msg: Msg): unit = send(msg, Actor.self)

  def ? : Msg = receive { case any => any }

  def poll: Option[Msg] = {
    Some(?)
  } orElse {
    None.asInstanceOf[Option[Msg]]
  }

  /**
   * Sends <code>msg</code> to this <code>Channel</code> and
   * awaits reply.
   */
  def !?(msg: Msg): Any = {
    Actor.self.freshReply()
    this ! msg
    Actor.self.reply.receiveFrom(receiver) {
      case x => x
    }
  }

  def !?(msec: long, msg: Msg): Option[Any] = {
    Debug.info("rpc with timeout "+msec)
    Actor.self.freshReply()
    this ! msg
    Actor.self.reply.receiveWithinFrom(msec)(receiver) {
      case TIMEOUT => None
      case x => Some(x)
    }
  }

  /**
   * Forwards <code>msg</code> to <code>this</code> keeping the
   * last sender as sender instead of <code>self</code>.
   */
  def forward(msg: Msg): unit = send(msg, receiver.sender)

  /**
   * Receives a message from this <code>Channel</code>.
   */
  def receive[R](f: PartialFunction[Msg, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()

/*
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      // acquire lock because we might call wait()
      else synchronized {
        isSuspended = true
        receiver.suspendActor()
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1)
      }) match {
        case Some(Pair(msg, sender)) => {
          received = Some(msg)
          receiver.pushSender(sender)
        }
        case None => {
          // acquire lock because we might call wait()
          this.synchronized {
            waitingFor = f.isDefinedAt
            isSuspended = true
            //val wp = Tuple4(f.isDefinedAt, None, None, Some(receiver))
            //waitingParts = wp :: waitingParts
            receiver.suspendActor()
          }
        }
      }

      waitingFor = waitingForNone
      isSuspended = false
    }
    val result = f(received.get)
    receiver.popSender()
    result
  }

  private[actors] def receiveFrom[R](r: Actor)(f: PartialFunction[Msg, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()

/*
      var q = messageQueue.dequeueFirst((item: MessageQueueResult[Msg]) => {
        waitingFor(item.msg) && item.sender == r
      })
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      else synchronized {
        isSuspended = true
        receiver.suspendActor()
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1) && p._2 == r
      }) match {
        case Some(Pair(msg, sender)) => {
          received = Some(msg)
          receiver.pushSender(sender)
        }
        case None => {
          // acquire lock because we might call wait()
          this.synchronized {
            waitingFor = f.isDefinedAt
            waitingForSender = r
            isSuspended = true
            receiver.suspendActor()
          }
        }
      }

      waitingFor = waitingForNone
      waitingForSender = null
      isSuspended = false
    }
    val result = f(received.get)
    receiver.popSender()
    result
  }

  /**
   * Receives a message from this <code>Channel</code>. If no
   * message could be received before <code>msec</code>
   * milliseconds elapsed, the <code>TIMEOUT</code> action is
   * executed if specified.
   */
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()

/*
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      else synchronized {
        isSuspended = true
        receiver.suspendActorFor(msec)
        if (received eq null)
          if (f.isDefinedAt(TIMEOUT)) {
            isSuspended = false
            val result = f(TIMEOUT)
            return result
          }
          else
            error("unhandled timeout")
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1)
      }) match {
        case Some(Pair(msg, sender)) => {
          received = Some(msg)
          receiver.pushSender(sender)
        }
        case None => {
          // acquire lock because we might call wait()
          this.synchronized {
            waitingFor = f.isDefinedAt
            isSuspended = true
            received = None
            receiver.suspendActorFor(msec)
            Debug.info("received: "+received)
            if (received.isEmpty) {
              Debug.info("no message received after "+msec+" millis")
              if (f.isDefinedAt(TIMEOUT)) {
                Debug.info("executing TIMEOUT action")
                isSuspended = false
                val result = f(TIMEOUT)
                return result
              }
              else
                error("unhandled timeout")
            }
          }
        }
      }

      waitingFor = waitingForNone
      isSuspended = false
    }
    val result = f(received.get)
    receiver.popSender()
    result
  }

  def receiveWithinFrom[R](msec: long)(r: Actor)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()

/*
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      else synchronized {
        waitingFor = f.isDefinedAt
        waitingForSender = r
        isSuspended = true
        receiver.suspendActorFor(msec)
        if (received eq null)
          if (f.isDefinedAt(TIMEOUT)) {
            isSuspended = false
            val result = f(TIMEOUT)
            return result
          }
          else
            error("unhandled timeout")
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1) && p._2 == r
      }) match {
        case Some(Pair(msg, sender)) => {
          received = Some(msg)
          receiver.pushSender(sender)
        }
        case None => {
          // acquire lock because we might call wait()
          this.synchronized {
            waitingFor = f.isDefinedAt
            waitingForSender = r
            isSuspended = true
            received = None
            receiver.suspendActorFor(msec)
            Debug.info("received: "+received)
            if (received.isEmpty) {
              Debug.info("no message received after "+msec+" millis")
              if (f.isDefinedAt(TIMEOUT)) {
                Debug.info("executing TIMEOUT action")
                isSuspended = false
                val result = f(TIMEOUT)
                return result
              }
              else
                error("unhandled timeout")
            }
          }
        }
      }

      waitingFor = waitingForNone
      waitingForSender = null
      isSuspended = false
    }
    val result = f(received.get)
    receiver.popSender()
    result
  }

  /**
   * <code>receive</code> for reactors.
   */
  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == receiver, "react on channel belonging to other actor")
    Scheduler.pendReaction
    receiver.synchronized {
      receiver.tick()

/*
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
        waitingFor = waitingForNone
        receiver.scheduleActor(f, received)
      }
      else synchronized {
        receiver.detachActor(f)
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1)
      }) match {
        case Some(Pair(msg, sender)) => {
          receiver.pushSender(sender)
          receiver.scheduleActor(f, msg)
        }
        case None => {
          this.synchronized {
            //Scheduler.detached(receiver)
            waitingFor = f.isDefinedAt
            //val wp = Tuple4(f.isDefinedAt, None, Some(f), None)
            //waitingParts = wp :: waitingParts
            receiver.detachActor(f)
          }
        }
      }

      throw new SuspendActorException
    }
  }

  /**
   * <code>receiveWithin</code> for reactors.
   */
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == receiver, "react on channel belonging to other actor")
    Scheduler.pendReaction
    receiver.synchronized {
      receiver.tick()

/*
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
        waitingFor = waitingForNone
        receiver.scheduleActor(f, received)
      }
      else synchronized {
        waitingFor = f.isDefinedAt
        TimerThread.requestTimeout(receiver.asInstanceOf[Reactor], f, msec)
        receiver.asInstanceOf[Reactor].timeoutPending = true
        receiver.detachActor(f)
      }
*/

      mailbox.dequeueFirst((p: Pair[Msg, Actor]) => {
        waitingFor(p._1)
      }) match {
        case Some(Pair(msg, sender)) => {
          received = Some(msg)
          receiver.pushSender(sender)
          waitingFor = waitingForNone
          receiver.scheduleActor(f, received.get)
        }
        case None => {
          this.synchronized {
            waitingFor = f.isDefinedAt
            TimerThread.requestTimeout(receiver, f, msec)
            receiver.timeoutPending = true
            receiver.detachActor(f)
          }
        }
      }

      throw new SuspendActorException
    }
  }

  /*
   * Prints contents of mailbox to standard out.
   * This is used for printing actor dumps.
   */
  private[actors] def printMailbox = {
    Console.print("[")
    val msgs = mailbox.elements
    if (msgs.hasNext)
      Console.print(msgs.next._1.toString())
    while (msgs.hasNext) {
      Console.print(", "+msgs.next._1.toString())
    }
    Console.println("]")
  }

}
