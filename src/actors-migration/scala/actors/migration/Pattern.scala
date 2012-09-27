package scala.actors.migration

import scala.actors._
import scala.concurrent.duration.Duration
import language.implicitConversions

object pattern {

  implicit def ask(ar: ActorRef): AskableActorRef =
    new AskableActorRef(ar)
}

/**
 * ActorRef with support for ask(?) operation.
 */
class AskableActorRef(val ar: ActorRef) extends ActorRef {

  def !(message: Any)(implicit sender: ActorRef = null): Unit = ar.!(message)(sender)

  def ?(message: Any)(implicit timeout: Timeout): scala.concurrent.Future[Any] = ar.?(message, timeout.duration)

  private[actors] def ?(message: Any, timeout: Duration): scala.concurrent.Future[Any] = ar.?(message, timeout)

  def forward(message: Any) = ar.forward(message)

  private[actors] def localActor: AbstractActor = ar.localActor
}
