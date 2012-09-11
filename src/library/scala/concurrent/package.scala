/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.concurrent.util.Duration
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
   *  @param body     the asychronous computation
   *  @param execctx  the execution context on which the future is run
   *  @return         the `Future` holding the result of the computation
   */
  def future[T](body: =>T)(implicit execctx: ExecutionContext): Future[T] = Future[T](body)

  /** Creates a promise object which can be completed with a value.
   *  
   *  @tparam T       the type of the value in the promise
   *  @param execctx  the execution context on which the promise is created on
   *  @return         the newly created `Promise` object
   */
  def promise[T]()(implicit execctx: ExecutionContext): Promise[T] = Promise[T]()

  /** Used to designate a piece of code which potentially blocks, allowing the BlockContext to adjust the runtime's behavior.
   *  Properly marking blocking code may improve performance or avoid deadlocks. 
   *
   *  If you have an `Awaitable` then you should use Await.result instead of `blocking`.
   *
   *  @param body         A piece of code which contains potentially blocking or long running calls.
   *
   *  Calling this method may throw the following exceptions:
   *  - CancellationException - if the computation was cancelled
   *  - InterruptedException - in the case that a wait within the blockable object was interrupted
   *  - TimeoutException - in the case that the blockable object timed out
   */
  @throws(classOf[Exception])
  def blocking[T](body: =>T): T = BlockContext.current.blockOn(body)(scala.concurrent.AwaitPermission)
}

package concurrent {
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
     * Await the "resolved" state of this Awaitable.
     * Invokes ready() on the awaitable, properly wrapped by a call to `scala.concurrent.blocking`.
     *
     * @param awaitable
     *        the `Awaitable` on which `ready` is to be called
     * @param atMost
     *        maximum wait time, which may be negative (no waiting is done),
     *        [[Duration.Inf]] for unbounded waiting, or a finite positive
     *        duration
     * @return the awaitable itself
     * @throws InterruptedException     if the wait call was interrupted
     * @throws TimeoutException         if after waiting for the specified time this Awaitable is still not ready
     * @throws IllegalArgumentException if `atMost` is [[Duration.Undefined]]
     */
    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    def ready[T](awaitable: Awaitable[T], atMost: Duration): awaitable.type =
      blocking(awaitable.ready(atMost)(AwaitPermission))
    
    /**
     * Await and return the result of this Awaitable, which is either of type T or a thrown exception (any Throwable).
     * Invokes result() on the awaitable, properly wrapped by a call to `scala.concurrent.blocking`.
     *
     * @param awaitable
     *        the `Awaitable` on which `result` is to be called
     * @param atMost
     *        maximum wait time, which may be negative (no waiting is done),
     *        [[Duration.Inf]] for unbounded waiting, or a finite positive
     *        duration
     * @return the value if the Awaitable was successful within the specific maximum wait time
     * @throws InterruptedException     if the wait call was interrupted
     * @throws TimeoutException         if after waiting for the specified time this Awaitable is still not ready
     * @throws IllegalArgumentException if `atMost` is [[Duration.Undefined]]
     */
    @throws(classOf[Exception])
    def result[T](awaitable: Awaitable[T], atMost: Duration): T =
      blocking(awaitable.result(atMost)(AwaitPermission))
  }
}
