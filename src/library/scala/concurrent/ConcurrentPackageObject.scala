/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.{ Executors, ExecutorService }
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.{ Duration, Try, Success, Failure }
import ConcurrentPackageObject._



/** This package object contains primitives for concurrent and parallel programming.
 */
abstract class ConcurrentPackageObject {
  /** A global execution environment for executing lightweight tasks.
   */
  lazy val executionContext =
    new impl.ExecutionContextImpl(getExecutorService)

  private[concurrent] def getExecutorService: AnyRef =
    if (util.Properties.isJavaAtLeast("1.6")) {
      val vendor = util.Properties.javaVmVendor
      if ((vendor contains "Oracle") || (vendor contains "Sun") || (vendor contains "Apple")) new ForkJoinPool
      else Executors.newCachedThreadPool()
    } else Executors.newCachedThreadPool()
  
  val handledFutureException: PartialFunction[Throwable, Throwable] = {
    case t: Throwable if isFutureThrowable(t) => t
  }

  // TODO rename appropriately and make public
  private[concurrent] def isFutureThrowable(t: Throwable) = t match {
    case e: Error                               => false
    case t: scala.util.control.ControlThrowable => false
    case i: InterruptedException                => false
    case _                                      => true
  }

  private[concurrent] def resolve[T](source: Try[T]): Try[T] = source match {
    case Failure(t: scala.runtime.NonLocalReturnControl[_]) => Success(t.value.asInstanceOf[T])
    case Failure(t: scala.util.control.ControlThrowable)    => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case Failure(t: InterruptedException)                   => Failure(new ExecutionException("Boxed InterruptedException", t))
    case Failure(e: Error)                                  => Failure(new ExecutionException("Boxed Error", e))
    case _                                                  => source
  }

  private[concurrent] def resolver[T] =
    resolverFunction.asInstanceOf[PartialFunction[Throwable, Try[T]]]

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

  @inline implicit final def int2durationops(x: Int): DurationOps = new DurationOps(x)
}

private[concurrent] object ConcurrentPackageObject {
  // TODO, docs, return type
  // Note that having this in the package object led to failures when
  // compiling a subset of sources; it seems that the wildcard is not
  // properly handled, and you get messages like "type _$1 defined twice".
  // This is consistent with other package object breakdowns.
  private val resolverFunction: PartialFunction[Throwable, Try[_]] = {
    case t: scala.runtime.NonLocalReturnControl[_] => Success(t.value)
    case t: scala.util.control.ControlThrowable    => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException                   => Failure(new ExecutionException("Boxed InterruptedException", t))
    case e: Error                                  => Failure(new ExecutionException("Boxed Error", e))
    case t                                         => Failure(t)
  }
}
