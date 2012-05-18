/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.{ Executors, Executor, ThreadFactory }
import scala.concurrent.forkjoin.{ ForkJoinPool, ForkJoinWorkerThread }
import scala.concurrent.util.Duration
import language.implicitConversions


/** This package object contains primitives for concurrent and parallel programming.
 */
abstract class ConcurrentPackageObject {
  /** A global execution environment for executing lightweight tasks.
   */
  lazy val defaultExecutionContext: ExecutionContext with Executor = impl.ExecutionContextImpl.fromExecutor(null: Executor)

  val currentExecutionContext = new ThreadLocal[ExecutionContext]
  
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

  /* concurrency constructs */

  /** Starts an asynchronous computation and returns a `Future` object with the result of that computation.
   *  
   *  The result becomes available once the asynchronous computation is completed.
   *  
   *  @tparam T       the type of the result
   *  @param body     the asychronous computation
   *  @param execctx  the execution context on which the future is run
   *  @return         the `Future` holding the result of the computation
   */
  def future[T](body: =>T)(implicit execctx: ExecutionContext = defaultExecutionContext): Future[T] =
    Future[T](body)

  /** Creates a promise object which can be completed with a value.
   *  
   *  @tparam T       the type of the value in the promise
   *  @param execctx  the execution context on which the promise is created on
   *  @return         the newly created `Promise` object
   */
  def promise[T]()(implicit execctx: ExecutionContext = defaultExecutionContext): Promise[T] =
    Promise[T]()

  /** Used to block on a piece of code which potentially blocks.
   *
   *  @param body         A piece of code which contains potentially blocking or long running calls.
   *
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def blocking[T](body: =>T): T =
    blocking(impl.Future.body2awaitable(body), Duration.Inf)

  /** Blocks on an awaitable object.
   *
   *  @param awaitable    An object with a `block` method which runs potentially blocking or long running calls.
   *
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  def blocking[T](awaitable: Awaitable[T], atMost: Duration): T = {
    currentExecutionContext.get match {
      case null => awaitable.result(atMost)(Await.canAwaitEvidence)
      case ec => ec.internalBlockingCall(awaitable, atMost)
    }
  }

  @inline implicit final def int2durationops(x: Int): DurationOps = new DurationOps(x)
}
