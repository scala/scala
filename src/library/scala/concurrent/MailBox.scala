/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.concurrent

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 12/03/2003
 */
//class MailBox with Monitor with LinkedListQueueCreator {
@deprecated("use actors instead")
class MailBox extends AnyRef with ListQueueCreator {

  type Message = AnyRef

  private abstract class PreReceiver {
    var msg: Message = null
    def isDefinedAt(msg: Message): Boolean
  }

  private class Receiver[A](receiver: PartialFunction[Message, A]) extends PreReceiver {

    def isDefinedAt(msg: Message) = receiver.isDefinedAt(msg)

    def receive(): A = synchronized {
      while (msg eq null) wait()
      receiver(msg)
    }

    def receiveWithin(msec: Long): A = synchronized {
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
  private def scanSentMsgs[A](receiver: Receiver[A]): Unit = synchronized {
    messageQueue.extractFirst(sent, msg => receiver.isDefinedAt(msg)) match {
      case None =>
        receivers = receiverQueue.append(receivers, receiver)
      case Some((msg, withoutMsg)) =>
        sent = withoutMsg
        receiver.msg = msg
    }
  }

  /**
  * First check whether a pending receiver is applicable to the sent
  * message. If yes, the receiver is notified. Otherwise the message
  * is appended to the linked list of sent messages.
  */
  def send(msg: Message): Unit = synchronized {
    receiverQueue.extractFirst(receivers, r => r.isDefinedAt(msg)) match {
      case None =>
        sent = messageQueue.append(sent, msg)
      case Some((receiver, withoutReceiver)) =>
        receivers = withoutReceiver
        receiver.msg = msg
        receiver synchronized { receiver.notify() }
    }
  }

  /**
  * Block until there is a message in the mailbox for which the processor
  * <code>f</code> is defined.
  */
  def receive[A](f: PartialFunction[Message, A]): A = {
    val r = new Receiver(f)
    scanSentMsgs(r)
    r.receive()
  }

  /**
  * Block until there is a message in the mailbox for which the processor
  * <code>f</code> is defined or the timeout is over.
  */
  def receiveWithin[A](msec: Long)(f: PartialFunction[Message, A]): A = {
    val r = new Receiver(f)
    scanSentMsgs(r)
    r.receiveWithin(msec)
  }

}



/**
* Module for dealing with queues.
*/
@deprecated("use actors instead")
trait QueueModule[A] {
  /** Type of queues. */
  type T
  /** Create an empty queue. */
  def make: T
  /** Append an element to a queue. */
  def append(l: T, x: A): T
  /** Extract an element satisfying a predicate from a queue. */
  def extractFirst(l: T, p: A => Boolean): Option[(A, T)]
}

/** Inefficient but simple queue module creator. */
@deprecated("use actors instead")
trait ListQueueCreator {
  def queueCreate[A]: QueueModule[A] = new QueueModule[A] {
    type T = List[A]
    def make: T = Nil
    def append(l: T, x: A): T = l ::: x :: Nil
    def extractFirst(l: T, p: A => Boolean): Option[(A, T)] =
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
@deprecated("use actors instead")
trait LinkedListQueueCreator {
  import scala.collection.mutable.LinkedList
  def queueCreate[A >: Null <: AnyRef]: QueueModule[A] = new QueueModule[A] {
    type T = (LinkedList[A], LinkedList[A]) // fst = the list, snd = last elem
    def make: T = {
      val l = new LinkedList[A](null, null)
      (l, l)
    }
    def append(l: T, x: A): T = {
      val atTail = new LinkedList(x, null)
      l._2 append atTail;
      (l._1, atTail)
    }
    def extractFirst(l: T, p: A => Boolean): Option[(A, T)] = {
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

