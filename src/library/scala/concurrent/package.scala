/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala



import java.util.concurrent.atomic.{ AtomicInteger }
import scala.util.{ Timeout, Duration }
import collection._
import scala.collection.generic.CanBuildFrom



/** This package object contains primitives for concurrent and parallel programming.
 */
package object concurrent {
  
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException
  
  /** A global execution environment for executing lightweight tasks.
   */
  lazy val executionContext =
    new akka.ExecutionContextImpl(java.util.concurrent.Executors.newCachedThreadPool())
  
  /** A global service for scheduling tasks for execution.
   */
  lazy val scheduler =
    new default.SchedulerImpl
  
  val handledFutureException: PartialFunction[Throwable, Throwable] = {
    case t: Throwable if isFutureThrowable(t) => t
  }
  
  // TODO rename appropriately and make public
  private[concurrent] def isFutureThrowable(t: Throwable) = t match {
    case e: Error => false
    case t: scala.util.control.ControlThrowable => false
    case i: InterruptedException => false
    case _ => true
  }
  
  private[concurrent] def resolve[T](source: Either[Throwable, T]): Either[Throwable, T] = source match {
    case Left(t: scala.runtime.NonLocalReturnControl[_]) => Right(t.value.asInstanceOf[T])
    case Left(t: scala.util.control.ControlThrowable) => Left(new ExecutionException("Boxed ControlThrowable", t))
    case Left(t: InterruptedException) => Left(new ExecutionException("Boxed InterruptedException", t))
    case Left(e: Error) => Left(new ExecutionException("Boxed Error", e))
    case _ => source
  }
  
  private val resolverFunction: PartialFunction[Throwable, Either[Throwable, _]] = {
    case t: scala.runtime.NonLocalReturnControl[_] => Right(t.value)
    case t: scala.util.control.ControlThrowable => Left(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException => Left(new ExecutionException("Boxed InterruptedException", t))
    case e: Error => Left(new ExecutionException("Boxed Error", e))
    case t => Left(t)
  }
  
  private[concurrent] def resolver[T] = resolverFunction.asInstanceOf[PartialFunction[Throwable, Either[Throwable, T]]]
  
  /* concurrency constructs */
  
  def future[T](body: =>T)(implicit execCtx: ExecutionContext = executionContext): Future[T] =
    execCtx future body
  
  def promise[T]()(implicit execCtx: ExecutionContext = executionContext): Promise[T] =
    execCtx promise
  
  /** Wraps a block of code into an awaitable object. */
  def body2awaitable[T](body: =>T) = new Awaitable[T] {
    def await(atMost: Duration)(implicit cb: CanAwait) = body
  }
  
  /** Used to block on a piece of code which potentially blocks.
   *  
   *  @param body         A piece of code which contains potentially blocking or long running calls.
   *  
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def blocking[T](atMost: Duration)(body: =>T)(implicit execCtx: ExecutionContext): T =
    executionContext.blocking(atMost)(body)
  
  /** Blocks on an awaitable object.
   *  
   *  @param awaitable    An object with a `block` method which runs potentially blocking or long running calls.
   *  
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def blocking[T](awaitable: Awaitable[T], atMost: Duration)(implicit execCtx: ExecutionContext = executionContext): T =
    executionContext.blocking(awaitable, atMost)
  
  object await {
    def ready[T](atMost: Duration)(awaitable: Awaitable[T])(implicit execCtx: ExecutionContext = executionContext): Awaitable[T] = {
      try blocking(awaitable, atMost)
      catch { case _ => }
      awaitable
    }
    
    def result[T](atMost: Duration)(awaitable: Awaitable[T])(implicit execCtx: ExecutionContext = executionContext): T = {
      blocking(awaitable, atMost)
    }
  }
  
}



package concurrent {
  
  /** A timeout exception.
   *  
   *  Futures are failed with a timeout exception when their timeout expires.
   *  
   *  Each timeout exception contains an origin future which originally timed out.
   */
  class FutureTimeoutException(origin: Future[_], message: String) extends TimeoutException(message) {
    def this(origin: Future[_]) = this(origin, "Future timed out.")
  }
  
  trait ExecutionContextBase {
  self: ExecutionContext =>
    
    private implicit val executionContext = self
    
    /** TODO some docs
     *  
     */
    def all[T, Coll[X] <: Traversable[X]](futures: Coll[Future[T]])(implicit cbf: CanBuildFrom[Coll[_], T, Coll[T]]): Future[Coll[T]] = {
      val buffer = new mutable.ArrayBuffer[T]
      val counter = new AtomicInteger(1) // how else could we do this?
      val p: Promise[Coll[T]] = promise[Coll[T]] // we need an implicit execctx in the signature
      var idx = 0
      
      def tryFinish() = if (counter.decrementAndGet() == 0) {
        val builder = cbf(futures)
        builder ++= buffer
        p success builder.result
      }
      
      for (f <- futures) {
        val currentIndex = idx
        buffer += null.asInstanceOf[T]
        counter.incrementAndGet()
        f onComplete {
          case Left(t) =>
            p tryFailure t
          case Right(v) =>
            buffer(currentIndex) = v
            tryFinish()
        }
        idx += 1
      }
      
      tryFinish()
      
      p.future
    }
    
    /** TODO some docs
     *  
     */
    def any[T](futures: Traversable[Future[T]]): Future[T] = {
      val p = promise[T]
      val completeFirst: Either[Throwable, T] => Unit = elem => p tryComplete elem
      
      futures foreach (_ onComplete completeFirst)
      
      p.future
    }
    
    /** TODO some docs
     *  
     */
    def find[T](futures: Traversable[Future[T]])(predicate: T => Boolean): Future[Option[T]] = {
      if (futures.isEmpty) Promise.successful[Option[T]](None).future
      else {
        val result = promise[Option[T]]
        val count = new AtomicInteger(futures.size)
        val search: Either[Throwable, T] => Unit = { 
          v => v match {
            case Right(r) => if (predicate(r)) result trySuccess Some(r)
            case _        =>
          }
          if (count.decrementAndGet() == 0) result trySuccess None
        }
        
        futures.foreach(_ onComplete search)

        result.future
      }
    }

  }
  
}


