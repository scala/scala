/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.dataflow

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ ConcurrentLinkedQueue, LinkedBlockingQueue }

import akka.event.EventHandler
import akka.actor.{ Actor, ActorRef }
import akka.actor.Actor._
import akka.dispatch.CompletableFuture
import akka.AkkaException
import akka.japi.{ Function, Effect }

/**
 * Implements Oz-style dataflow (single assignment) variables.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
object DataFlow {
  object Start
  object Exit

  class DataFlowVariableException(message: String, cause: Throwable = null) extends AkkaException(message, cause)

  /**
   * Executes the supplied thunk in another thread.
   */
  def thread(body: => Unit): Unit = spawn(body)

  /**
   * JavaAPI.
   * Executes the supplied Effect in another thread.
   */
  def thread(body: Effect): Unit = spawn(body.apply)

  /**
   * Executes the supplied function in another thread.
   */
  def thread[A <: AnyRef, R <: AnyRef](body: A => R) =
    actorOf(new ReactiveEventBasedThread(body)).start()

  /**
   * JavaAPI.
   * Executes the supplied Function in another thread.
   */
  def thread[A <: AnyRef, R <: AnyRef](body: Function[A, R]) =
    actorOf(new ReactiveEventBasedThread(body.apply)).start()

  private class ReactiveEventBasedThread[A <: AnyRef, T <: AnyRef](body: A => T)
    extends Actor {
    def receive = {
      case Exit    => self.stop()
      case message => self.reply(body(message.asInstanceOf[A]))
    }
  }

  private object DataFlowVariable {
    private sealed abstract class DataFlowVariableMessage
    private case class Set[T <: Any](value: T) extends DataFlowVariableMessage
    private object Get extends DataFlowVariableMessage
  }

  /**
   * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
   */
  @deprecated("Superceeded by Future and CompletableFuture as of 1.1", "1.1")
  sealed class DataFlowVariable[T <: Any](timeoutMs: Long) {
    import DataFlowVariable._

    def this() = this(1000 * 60)

    private val value = new AtomicReference[Option[T]](None)
    private val blockedReaders = new ConcurrentLinkedQueue[ActorRef]

    private class In[T <: Any](dataFlow: DataFlowVariable[T]) extends Actor {
      self.timeout = timeoutMs
      def receive = {
        case s@Set(v) =>
          if (dataFlow.value.compareAndSet(None, Some(v.asInstanceOf[T]))) {
            while (dataFlow.blockedReaders.peek ne null)
              dataFlow.blockedReaders.poll ! s
          } else throw new DataFlowVariableException(
            "Attempt to change data flow variable (from [" + dataFlow.value.get + "] to [" + v + "])")
        case Exit => self.stop()
      }
    }

    private class Out[T <: Any](dataFlow: DataFlowVariable[T]) extends Actor {
      self.timeout = timeoutMs
      private var readerFuture: Option[CompletableFuture[Any]] = None
      def receive = {
        case Get => dataFlow.value.get match {
          case Some(value) => self reply value
          case None        => readerFuture = self.senderFuture
        }
        case Set(v: T) => readerFuture.map(_ completeWithResult v)
        case Exit      => self.stop()
      }
    }

    private[this] val in = actorOf(new In(this)).start()

    /**
     * Sets the value of this variable (if unset) with the value of the supplied variable.
     */
    def <<(ref: DataFlowVariable[T]) {
      if (this.value.get.isEmpty) in ! Set(ref())
      else throw new DataFlowVariableException(
        "Attempt to change data flow variable (from [" + this.value.get + "] to [" + ref() + "])")
    }

    /**
     * JavaAPI.
     * Sets the value of this variable (if unset) with the value of the supplied variable.
     */
    def set(ref: DataFlowVariable[T]) { this << ref }

    /**
     * Sets the value of this variable (if unset).
     */
    def <<(value: T) {
      if (this.value.get.isEmpty) in ! Set(value)
      else throw new DataFlowVariableException(
        "Attempt to change data flow variable (from [" + this.value.get + "] to [" + value + "])")
    }

    /**
     * JavaAPI.
     * Sets the value of this variable (if unset) with the value of the supplied variable.
     */
    def set(value: T) { this << value }

    /**
     * Retrieves the value of variable, throws a DataFlowVariableException if it times out.
     */
    def get(): T = this()

    /**
     * Retrieves the value of variable, throws a DataFlowVariableException if it times out.
     */
    def apply(): T = {
      value.get getOrElse {
        val out = actorOf(new Out(this)).start()

        val result = try {
          blockedReaders offer out
          (out !! Get).as[T]
        } catch {
          case e: Exception =>
            EventHandler.error(e, this, e.getMessage)
            out ! Exit
            throw e
        }

        result.getOrElse(throw new DataFlowVariableException(
          "Timed out (after " + timeoutMs + " milliseconds) while waiting for result"))
      }
    }

    def shutdown() { in ! Exit }
  }
}
