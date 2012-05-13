/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.parallel



/** A future is a function without parameters that will block the caller if
 *  the parallel computation associated with the function is not completed.
 *  
 *  @tparam R   the type of the result
 *
 *  @since 2.9
 */
@deprecated("Use `scala.concurrent.Future` instead.", "2.10.0")
trait Future[@specialized +R] extends (() => R) {
  /** Returns a result once the parallel computation completes. If the
   *  computation produced an exception, an exception is forwarded.
   *
   *  '''Note:''' creating a circular dependency between futures by calling
   *  this method will result in a deadlock.
   *
   *  @return     the result
   *  @throws     the exception that was thrown during a parallel computation
   */
  def apply(): R

  /** Returns `true` if the parallel computation is completed.
   *
   *  @return     `true` if the parallel computation is completed, `false` otherwise
   */
  def isDone(): Boolean
}

