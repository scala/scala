/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.routing

import akka.actor.{ Actor, ActorRef }
import akka.actor.Actor._

object Routing {

  sealed trait RoutingMessage
  case class Broadcast(message: Any) extends RoutingMessage

  type PF[A, B] = PartialFunction[A, B]

  /**
   * Creates a new PartialFunction whose isDefinedAt is a combination
   * of the two parameters, and whose apply is first to call filter.apply
   * and then filtered.apply.
   */
  def filter[A, B](filter: PF[A, Unit], filtered: PF[A, B]): PF[A, B] = {
    case a: A if filtered.isDefinedAt(a) && filter.isDefinedAt(a) =>
      filter(a)
      filtered(a)
  }

  /**
   * Interceptor is a filter(x,y) where x.isDefinedAt is considered to be always true.
   */
  def intercept[A, B](interceptor: (A) => Unit, interceptee: PF[A, B]): PF[A, B] =
    filter({ case a if a.isInstanceOf[A] => interceptor(a) }, interceptee)

  /**
   * Creates a LoadBalancer from the thunk-supplied InfiniteIterator.
   */
  def loadBalancerActor(actors: => InfiniteIterator[ActorRef]): ActorRef =
    actorOf(new Actor with LoadBalancer {
      val seq = actors
    }).start()

  /**
   * Creates a Dispatcher given a routing and a message-transforming function.
   */
  def dispatcherActor(routing: PF[Any, ActorRef], msgTransformer: (Any) => Any): ActorRef =
    actorOf(new Actor with Dispatcher {
      override def transform(msg: Any) = msgTransformer(msg)
      def routes = routing
    }).start()

  /**
   * Creates a Dispatcher given a routing.
   */
  def dispatcherActor(routing: PF[Any, ActorRef]): ActorRef = actorOf(new Actor with Dispatcher {
    def routes = routing
  }).start()

  /**
   * Creates an actor that pipes all incoming messages to
   * both another actor and through the supplied function
   */
  def loggerActor(actorToLog: ActorRef, logger: (Any) => Unit): ActorRef =
    dispatcherActor({ case _ => actorToLog }, logger)
}
