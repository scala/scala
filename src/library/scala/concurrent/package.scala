/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala



import scala.util.{ Duration, Try, Success, Failure }



/** This package object contains primitives for concurrent and parallel programming.
 */
package object concurrent {
  
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException
  
  /** A global execution environment for executing lightweight tasks.
   */
  lazy val executionContext =
    new impl.ExecutionContextImpl(java.util.concurrent.Executors.newCachedThreadPool())
  
  /** A global service for scheduling tasks for execution.
   */
  // lazy val scheduler =
  //   new default.SchedulerImpl
  
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
  
  private[concurrent] def resolve[T](source: Try[T]): Try[T] = source match {
    case Failure(t: scala.runtime.NonLocalReturnControl[_]) => Success(t.value.asInstanceOf[T])
    case Failure(t: scala.util.control.ControlThrowable) => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case Failure(t: InterruptedException) => Failure(new ExecutionException("Boxed InterruptedException", t))
    case Failure(e: Error) => Failure(new ExecutionException("Boxed Error", e))
    case _ => source
  }
  
  // TODO, docs, return type
  private val resolverFunction: PartialFunction[Throwable, Try[_]] = {
    case t: scala.runtime.NonLocalReturnControl[_] => Success(t.value)
    case t: scala.util.control.ControlThrowable => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException => Failure(new ExecutionException("Boxed InterruptedException", t))
    case e: Error => Failure(new ExecutionException("Boxed Error", e))
    case t => Failure(t)
  }
  
  private[concurrent] def resolver[T] = resolverFunction.asInstanceOf[PartialFunction[Throwable, Try[T]]]
  
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
  
  /** Importing this object allows using some concurrency primitives
   *  on futures and promises that can yield nondeterministic programs.
   *  
   *  While program determinism is broken when using these primitives,
   *  some programs cannot be written without them (e.g. multiple client threads
   *  cannot send requests to a server thread through regular promises and futures).
   */
  object nondeterministic {
  }
  
  @inline implicit final def int2durationops(x: Int) = new DurationOps(x)
  
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
  
  final class DurationOps private[concurrent] (x: Int) {
    // TODO ADD OTHERS
    def ns = util.Duration.fromNanos(0)
  }
  
}


