/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.routing

import akka.actor.{ Actor, ActorRef }
import java.util.concurrent.ConcurrentSkipListSet
import scala.collection.convert.wrapAsScala._

sealed trait ListenerMessage
case class Listen(listener: ActorRef) extends ListenerMessage
case class Deafen(listener: ActorRef) extends ListenerMessage
case class WithListeners(f: (ActorRef) => Unit) extends ListenerMessage

/**
 * Listeners is a generic trait to implement listening capability on an Actor.
 * <p/>
 * Use the <code>gossip(msg)</code> method to have it sent to the listeners.
 * <p/>
 * Send <code>Listen(self)</code> to start listening.
 * <p/>
 * Send <code>Deafen(self)</code> to stop listening.
 * <p/>
 * Send <code>WithListeners(fun)</code> to traverse the current listeners.
 */
trait Listeners { self: Actor =>
  private val listeners = new ConcurrentSkipListSet[ActorRef]

  protected def listenerManagement: Receive = {
    case Listen(l)        => listeners add l
    case Deafen(l)        => listeners remove l
    case WithListeners(f) => listeners foreach f
  }

  protected def gossip(msg: Any) = listeners foreach (_ ! msg)
}
