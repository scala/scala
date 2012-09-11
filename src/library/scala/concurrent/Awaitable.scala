/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import scala.concurrent.util.Duration



trait Awaitable[+T] {
  /**
   * Await the "resolved" state of this Awaitable.
   * This method should not be called directly.
   *
   * @param atMost
   *        maximum wait time, which may be negative (no waiting is done),
   *        [[Duration.Inf]] for unbounded waiting, or a finite positive
   *        duration
   * @return the Awaitable itself
   * @throws InterruptedException     if the wait call was interrupted
   * @throws TimeoutException         if after waiting for the specified time this Awaitable is still not ready
   * @throws IllegalArgumentException if `atMost` is [[Duration.Undefined]]
   */
  @throws(classOf[TimeoutException])
  @throws(classOf[InterruptedException])
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type
  
  /**
   * Await and return the result of this Awaitable, which is either of type T or a thrown exception (any Throwable).
   * This method should not be called directly.
   *
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
  def result(atMost: Duration)(implicit permit: CanAwait): T
}



