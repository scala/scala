/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.concurrent.duration.Duration
import scala.annotation.implicitNotFound

/** This package object contains primitives for concurrent and parallel programming.
 */
package object concurrent {
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException

  /** Starts an asynchronous computation and returns a `Future` object with the result of that computation.
   *
   *  The result becomes available once the asynchronous computation is completed.
   *
   *  @tparam T       the type of the result
   *  @param body     the asynchronous computation
   *  @param executor the execution context on which the future is run
   *  @return         the `Future` holding the result of the computation
   */
  @deprecated("Use `Future { ... }` instead.", "2.11.0")
  // removal planned for 2.13.0
  def future[T](body: =>T)(implicit @deprecatedName('execctx) executor: ExecutionContext): Future[T] = Future[T](body)

  /** Creates a promise object which can be completed with a value or an exception.
   *
   *  @tparam T       the type of the value in the promise
   *  @return         the newly created `Promise` object
   */
  @deprecated("Use `Promise[T]()` instead.", "2.11.0")
  // removal planned for 2.13.0
  def promise[T](): Promise[T] = Promise[T]()

  /** Used to designate a piece of code which potentially blocks, allowing the current [[BlockContext]] to adjust
   *  the runtime's behavior.
   *  Properly marking blocking code may improve performance or avoid deadlocks.
   *
   *  Blocking on an [[Awaitable]] should be done using [[Await.result]] instead of `blocking`.
   *
   *  @param body         A piece of code which contains potentially blocking or long running calls.
   *  @throws `CancellationException` if the computation was cancelled
   *  @throws `InterruptedException` in the case that a wait within the blocking `body` was interrupted
   */
  @throws(classOf[Exception])
  def blocking[T](body: =>T): T = BlockContext.current.blockOn(body)(scala.concurrent.AwaitPermission)
}

package concurrent {
  /**
   * This marker trait is used by [[Await]] to ensure that [[Awaitable.ready]] and [[Awaitable.result]]
   * are not directly called by user code. An implicit instance of this trait is only available when
   * user code is currently calling the methods on [[Await]].
   */
  @implicitNotFound("Don't call `Awaitable` methods directly, use the `Await` object.")
  sealed trait CanAwait

  /**
   * Internal usage only, implementation detail.
   */
  private[concurrent] object AwaitPermission extends CanAwait

  /**
   * `Await` is what is used to ensure proper handling of blocking for `Awaitable` instances.
   */
  object Await {
    /**
     * Await the "completed" state of an `Awaitable`.
     *
     * Although this method is blocking, the internal use of [[scala.concurrent.blocking blocking]] ensures that
     * the underlying [[ExecutionContext]] is prepared to properly manage the blocking.
     *
     * @param  awaitable
     *         the `Awaitable` to be awaited
     * @param  atMost
     *         maximum wait time, which may be negative (no waiting is done),
     *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
     *         duration
     * @return the `awaitable`
     * @throws InterruptedException     if the current thread is interrupted while waiting
     * @throws TimeoutException         if after waiting for the specified time this `Awaitable` is still not ready
     * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
     */
    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    def ready[T](awaitable: Awaitable[T], atMost: Duration): awaitable.type =
      blocking(awaitable.ready(atMost)(AwaitPermission))

    /**
     * Await and return the result (of type `T`) of an `Awaitable`.
     *
     * Although this method is blocking, the internal use of [[scala.concurrent.blocking blocking]] ensures that
     * the underlying [[ExecutionContext]] to properly detect blocking and ensure that there are no deadlocks.
     *
     * @param  awaitable
     *         the `Awaitable` to be awaited
     * @param  atMost
     *         maximum wait time, which may be negative (no waiting is done),
     *         [[scala.concurrent.duration.Duration.Inf Duration.Inf]] for unbounded waiting, or a finite positive
     *         duration
     * @return the result value if `awaitable` is completed within the specific maximum wait time
     * @throws InterruptedException     if the current thread is interrupted while waiting
     * @throws TimeoutException         if after waiting for the specified time `awaitable` is still not ready
     * @throws IllegalArgumentException if `atMost` is [[scala.concurrent.duration.Duration.Undefined Duration.Undefined]]
     */
    @throws(classOf[Exception])
    def result[T](awaitable: Awaitable[T], atMost: Duration): T =
      blocking(awaitable.result(atMost)(AwaitPermission))
  }
}
