/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.dispatch

import akka.actor.{ Actor, ActorRef }
import akka.config.Config.config
import akka.util.Duration

import java.util.Queue
import java.util.concurrent.{ ConcurrentLinkedQueue, BlockingQueue, TimeUnit, LinkedBlockingQueue }
import akka.actor
import java.util.concurrent.atomic.AtomicReference

/**
 * Dedicates a unique thread for each actor passed in as reference. Served through its messageQueue.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class ThreadBasedDispatcher(_actor: ActorRef, _mailboxType: MailboxType)
  extends ExecutorBasedEventDrivenDispatcher(
    _actor.uuid.toString, Dispatchers.THROUGHPUT, -1, _mailboxType, ThreadBasedDispatcher.oneThread) {

  private[akka] val owner = new AtomicReference[ActorRef](_actor)

  def this(actor: ActorRef) =
    this(actor, UnboundedMailbox()) // For Java API

  def this(actor: ActorRef, capacity: Int) =
    this(actor, BoundedMailbox(capacity)) //For Java API

  def this(actor: ActorRef, capacity: Int, pushTimeOut: Duration) = //For Java API
    this(actor, BoundedMailbox(capacity, pushTimeOut))

  override def register(actorRef: ActorRef) = {
    val actor = owner.get()
    if ((actor ne null) && actorRef != actor) throw new IllegalArgumentException("Cannot register to anyone but " + actor)
    owner.compareAndSet(null, actorRef) //Register if unregistered
    super.register(actorRef)
  }

  override def unregister(actorRef: ActorRef) = {
    super.unregister(actorRef)
    owner.compareAndSet(actorRef, null) //Unregister (prevent memory leak)
  }
}

object ThreadBasedDispatcher {
  val oneThread: ThreadPoolConfig = ThreadPoolConfig(allowCorePoolTimeout = true, corePoolSize = 1, maxPoolSize = 1)
}

