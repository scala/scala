/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import java.util.concurrent.TimeUnit.{ NANOSECONDS, MILLISECONDS }
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.concurrent.{Awaitable, ExecutionContext, resolve, resolver, blocking, CanAwait, TimeoutException}
//import scala.util.continuations._
import scala.util.Duration
import scala.util.Try
import scala.util
import scala.annotation.tailrec
//import scala.concurrent.NonDeterministic



trait Promise[T] extends scala.concurrent.Promise[T] with Future[T] {

  def future = this

  def newPromise[S]: Promise[S] = executor promise

  // TODO refine answer and return types here from Any to type parameters
  // then move this up in the hierarchy
  /*
  final def <<(value: T): Future[T] @cps[Future[Any]] = shift {
    cont: (Future[T] => Future[Any]) =>
    cont(complete(Right(value)))
  }

  final def <<(other: Future[T]): Future[T] @cps[Future[Any]] = shift {
    cont: (Future[T] => Future[Any]) =>
    val p = executor.promise[Any]
    val thisPromise = this

    thisPromise completeWith other
    thisPromise onComplete { v =>
      try {
        p completeWith cont(thisPromise)
      } catch {
        case e => p complete resolver(e)
      }
    }

    p.future
  }
  */
  // TODO finish this once we introduce something like dataflow streams

  /*
  final def <<(stream: PromiseStreamOut[T]): Future[T] @cps[Future[Any]] = shift { cont: (Future[T] => Future[Any]) =>
    val fr = executor.promise[Any]
    val f = stream.dequeue(this)
    f.onComplete { _ =>
      try {
        fr completeWith cont(f)
      } catch {
        case e =>
          fr failure e
      }
    }
    fr
  }
  */

}


object Promise {
  def dur2long(dur: Duration): Long = if (dur.isFinite) dur.toNanos else Long.MaxValue

  def EmptyPending[T](): FState[T] = emptyPendingValue.asInstanceOf[FState[T]]

  /** Represents the internal state.
   */
  sealed trait FState[+T] { def value: Option[Try[T]] }

  case class Pending[T](listeners: List[Try[T] => Any] = Nil) extends FState[T] {
    def value: Option[Try[T]] = None
  }

  case class Success[T](value: Option[util.Success[T]] = None) extends FState[T] {
    def result: T = value.get.get
  }

  case class Failure[T](value: Option[util.Failure[T]] = None) extends FState[T] {
    def exception: Throwable = value.get.exception
  }

  private val emptyPendingValue = Pending[Nothing](Nil)

  /** Default promise implementation.
   */
  class DefaultPromise[T](implicit val executor: ExecutionContextImpl) extends AbstractPromise with Promise[T] {
  self =>

    updater.set(this, Promise.EmptyPending())

    protected final def tryAwait(atMost: Duration): Boolean = {
      @tailrec
      def awaitUnsafe(waitTimeNanos: Long): Boolean = {
        if (value.isEmpty && waitTimeNanos > 0) {
          val ms = NANOSECONDS.toMillis(waitTimeNanos)
          val ns = (waitTimeNanos % 1000000l).toInt // as per object.wait spec
          val start = System.nanoTime()
          try {
            synchronized {
              while (value.isEmpty) wait(ms, ns)
            }
          } catch {
            case e: InterruptedException =>
          }

          awaitUnsafe(waitTimeNanos - (System.nanoTime() - start))
        } else
          value.isDefined
      }

      executor.blocking(concurrent.body2awaitable(awaitUnsafe(dur2long(atMost))), Duration.fromNanos(0))
    }

    private def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
      if (value.isDefined || tryAwait(atMost)) this
      else throw new TimeoutException("Futures timed out after [" + atMost.toMillis + "] milliseconds")

    def await(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get match {
        case util.Failure(e)  => throw e
        case util.Success(r) => r
      }

    def value: Option[Try[T]] = getState.value

    @inline
    private[this] final def updater = AbstractPromise.updater.asInstanceOf[AtomicReferenceFieldUpdater[AbstractPromise, FState[T]]]

    @inline
    protected final def updateState(oldState: FState[T], newState: FState[T]): Boolean = updater.compareAndSet(this, oldState, newState)

    @inline
    protected final def getState: FState[T] = updater.get(this)

    def tryComplete(value: Try[T]): Boolean = {
      val callbacks: List[Try[T] => Any] = {
        try {
          @tailrec
          def tryComplete(v: Try[T]): List[Try[T] => Any] = {
            getState match {
              case cur @ Pending(listeners) =>
                if (updateState(cur, if (v.isFailure) Failure(Some(v.asInstanceOf[util.Failure[T]])) else Success(Some(v.asInstanceOf[util.Success[T]])))) listeners
                else tryComplete(v)
              case _ => null
            }
          }
          tryComplete(resolve(value))
        } finally {
          synchronized { notifyAll() } // notify any blockers from `tryAwait`
        }
      }

      callbacks match {
        case null             => false
        case cs if cs.isEmpty => true
        case cs               =>
          executor dispatchFuture {
            () => cs.foreach(f => notifyCompleted(f, value))
          }
          true
      }
    }

    def onComplete[U](func: Try[T] => U): this.type = {
      @tailrec // Returns whether the future has already been completed or not
      def tryAddCallback(): Boolean = {
        val cur = getState
        cur match {
          case _: Success[_] | _: Failure[_] => true
          case p: Pending[_] =>
            val pt = p.asInstanceOf[Pending[T]]
            if (updateState(pt, pt.copy(listeners = func :: pt.listeners))) false else tryAddCallback()
        }
      }

      if (tryAddCallback()) {
        val result = value.get
        executor dispatchFuture {
          () => notifyCompleted(func, result)
        }
      }

      this
    }

    private final def notifyCompleted(func: Try[T] => Any, result: Try[T]) {
      try {
        func(result)
      } catch {
        case e => executor.reportFailure(e)
      }
    }
  }

  /** An already completed Future is given its result at creation.
   *
   *  Useful in Future-composition when a value to contribute is already available.
   */
  final class KeptPromise[T](suppliedValue: Try[T])(implicit val executor: ExecutionContextImpl) extends Promise[T] {
    val value = Some(resolve(suppliedValue))

    def tryComplete(value: Try[T]): Boolean = false

    def onComplete[U](func: Try[T] => U): this.type = {
      val completedAs = value.get
      executor dispatchFuture {
        () => func(completedAs)
      }
      this
    }

    private def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

    def await(atMost: Duration)(implicit permit: CanAwait): T = value.get match {
      case util.Failure(e)  => throw e
      case util.Success(r) => r
    }
  }

}
















