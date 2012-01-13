/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.akka



import java.util.concurrent.TimeUnit.{ NANOSECONDS, MILLISECONDS }
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.concurrent.{Awaitable, ExecutionContext, resolver, blocking, CanAwait, TimeoutException}
import scala.util.continuations._
import scala.util.Duration
import scala.annotation.tailrec



trait Promise[T] extends scala.concurrent.Promise[T] with Future[T] {
  
  // TODO refine answer and return types here from Any to type parameters
  // then move this up in the hierarchy
  
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
  
  def EmptyPending[T](): FState[T] = emptyPendingValue.asInstanceOf[FState[T]]
  
  /** Represents the internal state.
   */
  sealed trait FState[+T] { def value: Option[Either[Throwable, T]] }
  
  case class Pending[T](listeners: List[Either[Throwable, T] ⇒ Unit] = Nil) extends FState[T] {
    def value: Option[Either[Throwable, T]] = None
  }
  
  case class Success[T](value: Option[Either[Throwable, T]] = None) extends FState[T] {
    def result: T = value.get.right.get
  }
  
  case class Failure[T](value: Option[Either[Throwable, T]] = None) extends FState[T] {
    def exception: Throwable = value.get.left.get
  }
  
  private val emptyPendingValue = Pending[Nothing](Nil)
  
  /* default promise implementation */
  abstract class DefaultPromise[T](implicit val executor: ExecutionContext) extends AbstractPromise with Promise[T] {
  self =>
    
    updater.set(this, Promise.EmptyPending())
    
    protected final def tryAwait(atMost: Duration): Boolean = {
      @tailrec
      def awaitUnsafe(waitTimeNanos: Long): Boolean = {
        if (value.isEmpty && waitTimeNanos > 0) {
          val ms = NANOSECONDS.toMillis(waitTimeNanos)
          val ns = (waitTimeNanos % 1000000l).toInt // as per object.wait spec
          val start = System.nanoTime()
          try { synchronized { if (value.isEmpty) wait(ms, ns) } } catch { case e: InterruptedException ⇒ }
          
          awaitUnsafe(waitTimeNanos - (System.nanoTime() - start))
        } else
          value.isDefined
      }
      
      executor.blockingCall(concurrent.body2awaitable(awaitUnsafe(dur2long(atMost))))
    }
    
    private def ready(atMost: Duration)(implicit permit: CanAwait): this.type =
      if (value.isDefined || tryAwait(atMost)) this
      else throw new TimeoutException("Futures timed out after [" + atMost.toMillis + "] milliseconds")
    
    def await(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get match {
        case Left(e)  => throw e
        case Right(r) => r
      }
    
    def value: Option[Either[Throwable, T]] = getState.value
    
    @inline
    private[this] final def updater = AbstractPromise.updater.asInstanceOf[AtomicReferenceFieldUpdater[AbstractPromise, FState[T]]]
    
    @inline
    protected final def updateState(oldState: FState[T], newState: FState[T]): Boolean = updater.compareAndSet(this, oldState, newState)
    
    @inline
    protected final def getState: FState[T] = updater.get(this)
    
    /*
    def tryComplete(value: Either[Throwable, T]): Boolean = {
      val callbacks: List[Either[Throwable, T] => Unit] = {
        try {
          @tailrec
          def tryComplete(v: Either[Throwable, T]): List[Either[Throwable, T] => Unit] = {
            getState match {
              case cur @ Pending(listeners) =>
              if (updateState(cur, if (v.isLeft) Failure(Some(v)) else Success(Some(v)))) listeners
                                   else tryComplete(v)
              case _ => null
            }
          }
          tryComplete(resolve(value))
        } finally {
          synchronized { notifyAll() } //Notify any evil blockers
        }
      }
      
      callbacks match {
        case null             => false
        case cs if cs.isEmpty => true
        case cs               => Future.dispatchTask(() => cs.foreach(f => notifyCompleted(f, value))); true
      }
    }
    
    def onComplete(func: Either[Throwable, T] => Unit): this.type = {
      @tailrec //Returns whether the future has already been completed or not
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
        Future.dispatchTask(() => notifyCompleted(func, result))
      }
      
      this
    }
    
    private final def notifyCompleted(func: Either[Throwable, T] => Unit, result: Either[Throwable, T]) {
      try { func(result) } catch { case e => logError("Future onComplete-callback raised an exception", e) }
    }
    */
  }
  
  /*
  /**
   * An already completed Future is seeded with it's result at creation, is useful for when you are participating in
   * a Future-composition but you already have a value to contribute.
   */
  final class KeptPromise[T](suppliedValue: Either[Throwable, T])(implicit val executor: ExecutionContext) extends Promise[T] {
    val value = Some(resolve(suppliedValue))

    def tryComplete(value: Either[Throwable, T]): Boolean = false
    def onComplete(func: Either[Throwable, T] => Unit): this.type = {
      val completedAs = value.get
      Future dispatchTask (() => func(completedAs))
      this
    }

    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this
    def result(atMost: Duration)(implicit permit: CanAwait): T = value.get match {
      case Left(e)  => throw e
      case Right(r) => r
    }
  }
  */
}
















