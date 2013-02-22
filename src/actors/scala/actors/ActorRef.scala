package scala.actors

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration

/**
 * Trait used for migration of Scala actors to Akka.
 */
@deprecated("ActorRef ought to be used only with the Actor Migration Kit.", "2.10.0")
trait ActorRef {

  /**
   * Sends a one-way asynchronous message. E.g. fire-and-forget semantics.
   * <p/>
   *
   * If invoked from within an actor then the actor reference is implicitly passed on as the implicit 'sender' argument.
   * <p/>
   *
   * This actor 'sender' reference is then available in the receiving actor in the 'sender' member variable,
   * if invoked from within an Actor. If not then no sender is available.
   * <pre>
   *   actor ! message
   * </pre>
   * <p/>
   */
  def !(message: Any)(implicit sender: ActorRef = null): Unit

  /**
   * Sends a message asynchronously, returning a future which may eventually hold the reply.
   */
  private[actors] def ?(message: Any, timeout: Duration): scala.concurrent.Future[Any]

  /**
   * Forwards the message and passes the original sender actor as the sender.
   * <p/>
   * Works with '!' and '?'.
   */
  def forward(message: Any)

  private[actors] def localActor: AbstractActor

}

/**
 * This is what is used to complete a Future that is returned from an ask/? call,
 * when it times out.
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
class AskTimeoutException(message: String, cause: Throwable) extends TimeoutException {
  def this(message: String) = this(message, null: Throwable)
}
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
object PoisonPill
