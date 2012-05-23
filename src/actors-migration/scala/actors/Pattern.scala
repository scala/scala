package scala.actors

import scala.concurrent.util.Duration

object pattern {

  implicit def askSupport(ar: ActorRef): AskableActorRef =
    new AskableActorRef(ar)
}

/**
 * ActorRef with support for ask(?) operation.
 */
class AskableActorRef(val ar: ActorRef) extends ActorRef {

  def !(message: Any)(implicit sender: ActorRef = null): Unit = ar.!(message)(sender)

  def ?(message: Any)(timeout: Timeout): Future[Any] = ar.?(message, timeout.duration)

  private[actors] def ?(message: Any, timeout: Duration): Future[Any] = ar.?(message, timeout)

  def forward(message: Any) = ar.forward(message)

  private[actors] def localActor: AbstractActor = ar.localActor
}