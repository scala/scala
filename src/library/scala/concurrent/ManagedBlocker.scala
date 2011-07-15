/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

/** The `ManagedBlocker` trait...
 *
 *  @author Philipp Haller
 */
trait ManagedBlocker {

  /**
   * Possibly blocks the current thread, for example waiting for
   * a lock or condition.
   *
   * @return true if no additional blocking is necessary (i.e.,
   *         if `isReleasable` would return `true`).
   * @throws InterruptedException if interrupted while waiting
   *         (the method is not required to do so, but is allowed to).
   */
  def block(): Boolean

  /**
   * Returns `true` if blocking is unnecessary.
   */
  def isReleasable: Boolean

}
