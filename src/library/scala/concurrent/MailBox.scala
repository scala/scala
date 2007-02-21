/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 12/03/2003
 */
//class MailBox with Monitor with LinkedListQueueCreator {
class MailBox extends AnyRef with ListQueueCreator {

  type Message = AnyRef

  private abstract class PreReceiver {
    var msg: Message = null
    def isDefinedAt(msg: Message): boolean
  }

  private class Receiver[a](receiver: PartialFunction[Message, a]) extends PreReceiver {

    def isDefinedAt(msg: Message) = receiver.isDefinedAt(msg)

    def receive(): a = synchronized {
      while (msg eq null) wait()
      receiver(msg)
    }

    def receiveWithin(msec: long): a = synchronized {
      if (msg eq null) wait(msec)
      receiver(if (msg ne null) msg else TIMEOUT)
    }
  }

  private val messageQueue = queueCreate[Message]
  private val receiverQueue = queueCreate[PreReceiver]

  /** Unconsumed messages. */
  private var sent = messageQueue.make

  /** Pending receivers. */
  private var receivers = receiverQueue.make

  /**
  * Check whether the receiver can be applied to an unconsumed message.
  * If yes, the message is extracted and associated with the receiver.
  * Otherwise the receiver is appended to the list of pending receivers.
  */
  private def scanSentMsgs[a](receiver: Receiver[a]): unit = synchronized {
    messageQueue.extractFirst(sent, msg => receiver.isDefinedAt(msg)) match {
      case None => receivers = receiverQueue.append(receivers, receiver)
      case Some((msg, withoutMsg)) => {
        sent = withoutMsg
        receiver.msg = msg
      }
    }
  }

  /**
  * First check whether a pending receiver is applicable to the sent
  * message. If yes, the receiver is notified. Otherwise the message
  * is appended to the linked list of sent messages.
  */
  def send(msg: Message): unit = synchronized {
    receiverQueue.extractFirst(receivers, r => r.isDefinedAt(msg)) match {
      case None => sent = messageQueue.append(sent, msg)
      case Some((receiver, withoutReceiver)) => {
        receivers = withoutReceiver
        receiver.msg = msg
        receiver synchronized { receiver.notify() }
      }
    }
  }

  /**
  * Block until there is a message in the mailbox for which the processor
  * <code>f</code> is defined.
  */
  def receive[a](f: PartialFunction[Message, a]): a = {
    val r = new Receiver(f)
    scanSentMsgs(r)
    r.receive()
  }

  /**
  * Block until there is a message in the mailbox for which the processor
  * <code>f</code> is defined or the timeout is over.
  */
  def receiveWithin[a](msec: long)(f: PartialFunction[Message, a]): a = {
    val r = new Receiver(f)
    scanSentMsgs(r)
    r.receiveWithin(msec)
  }

}

/////////////////////////////////////////////////////////////////

/**
* Module for dealing with queues.
*/
trait QueueModule[a] {
  /** Type of queues. */
  type t
  /** Create an empty queue. */
  def make: t
  /** Append an element to a queue. */
  def append(l: t, x: a): t
  /** Extract an element satisfying a predicate from a queue. */
  def extractFirst(l: t, p: a => boolean): Option[(a, t)]
}

/** Inefficient but simple queue module creator. */
trait ListQueueCreator {
  def queueCreate[a]: QueueModule[a] = new QueueModule[a] {
    type t = List[a]
    def make: t = Nil
    def append(l: t, x: a): t = l ::: x :: Nil
    def extractFirst(l: t, p: a => boolean): Option[(a, t)] =
      l match {
        case Nil => None
        case head :: tail =>
          if (p(head))
            Some((head, tail))
          else
            extractFirst(tail, p) match {
              case None => None
              case Some((x, without_x)) => Some((x, head :: without_x))
            }
      }
  }
}

/** Efficient queue module creator based on linked lists. */
trait LinkedListQueueCreator {
  import scala.collection.mutable.LinkedList
  def queueCreate[a >: Null <: AnyRef]: QueueModule[a] = new QueueModule[a] {
    type t = (LinkedList[a], LinkedList[a]) // fst = the list, snd = last elem
    def make: t = {
      val l = new LinkedList[a](null, null)
      (l, l)
    }
    def append(l: t, x: a): t = {
      val atTail = new LinkedList(x, null)
      l._2 append atTail;
      (l._1, atTail)
    }
    def extractFirst(l: t, p: a => boolean): Option[(a, t)] = {
      var xs = l._1
      var xs1 = xs.next
      while ((xs1 ne null) && !p(xs1.elem)) {
        xs = xs1
        xs1 = xs1.next
      }
      if (xs1 ne null) {
        xs.next = xs1.next
        if (xs.next eq null)
          Some((xs1.elem, (l._1, xs)))
        else
          Some((xs1.elem, l))
      }
      else
        None
    }
  }
}

