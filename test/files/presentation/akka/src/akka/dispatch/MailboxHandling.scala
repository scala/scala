/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.dispatch

import akka.AkkaException

import java.util.{ Comparator, PriorityQueue }
import java.util.concurrent._
import akka.util._

class MessageQueueAppendFailedException(message: String, cause: Throwable = null) extends AkkaException(message, cause)

/**
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
trait MessageQueue {
  val dispatcherLock = new SimpleLock
  val suspended = new SimpleLock
  def enqueue(handle: MessageInvocation)
  def dequeue(): MessageInvocation
  def size: Int
  def isEmpty: Boolean
}

/**
 * Mailbox configuration.
 */
sealed trait MailboxType

case class UnboundedMailbox() extends MailboxType
case class BoundedMailbox(
  val capacity: Int = { if (Dispatchers.MAILBOX_CAPACITY < 0) Int.MaxValue else Dispatchers.MAILBOX_CAPACITY },
  val pushTimeOut: Duration = Dispatchers.MAILBOX_PUSH_TIME_OUT) extends MailboxType {
  if (capacity < 0) throw new IllegalArgumentException("The capacity for BoundedMailbox can not be negative")
  if (pushTimeOut eq null) throw new IllegalArgumentException("The push time-out for BoundedMailbox can not be null")
}

trait UnboundedMessageQueueSemantics extends MessageQueue { self: BlockingQueue[MessageInvocation] =>
  @inline
  final def enqueue(handle: MessageInvocation): Unit = this add handle
  @inline
  final def dequeue(): MessageInvocation = this.poll()
}

trait BoundedMessageQueueSemantics extends MessageQueue { self: BlockingQueue[MessageInvocation] =>
  def pushTimeOut: Duration

  final def enqueue(handle: MessageInvocation) {
    if (pushTimeOut.length > 0) {
      this.offer(handle, pushTimeOut.length, pushTimeOut.unit) || {
        throw new MessageQueueAppendFailedException("Couldn't enqueue message " + handle + " to " + toString)
      }
    } else this put handle
  }

  @inline
  final def dequeue(): MessageInvocation = this.poll()
}

class DefaultUnboundedMessageQueue extends LinkedBlockingQueue[MessageInvocation] with UnboundedMessageQueueSemantics

class DefaultBoundedMessageQueue(capacity: Int, val pushTimeOut: Duration) extends LinkedBlockingQueue[MessageInvocation](capacity) with BoundedMessageQueueSemantics

class UnboundedPriorityMessageQueue(cmp: Comparator[MessageInvocation]) extends PriorityBlockingQueue[MessageInvocation](11, cmp) with UnboundedMessageQueueSemantics

class BoundedPriorityMessageQueue(capacity: Int, val pushTimeOut: Duration, cmp: Comparator[MessageInvocation]) extends BoundedBlockingQueue[MessageInvocation](capacity, new PriorityQueue[MessageInvocation](11, cmp)) with BoundedMessageQueueSemantics
