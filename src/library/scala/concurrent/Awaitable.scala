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
   * Should throw [[scala.concurrent.TimeoutException]] if it times out
   * This method should not be called directly.
   */
  @throws(classOf[TimeoutException])
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type
  
  /**
   * Throws exceptions if it cannot produce a T within the specified time.
   * This method should not be called directly.
   */
  @throws(classOf[Exception])
  def result(atMost: Duration)(implicit permit: CanAwait): T
}



