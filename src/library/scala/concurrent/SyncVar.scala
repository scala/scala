/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.TimeUnit

/** A class to provide safe concurrent access to a mutable cell.
 *  All methods are synchronized.
 *
 *  @tparam A type of the contained value
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class SyncVar[A] {
  private var isDefined: Boolean = false
  private var value: Option[A] = None

  /**
   * Waits for this SyncVar to become defined and returns
   * the result, without modifying the stored value.
   *
   * @return value that is held in this container
   */
  def get: A = synchronized {
    while (!isDefined) wait()
    value.get
  }

  /** Waits `timeout` millis. If `timeout <= 0` just returns 0.
    * It never returns negative results.
    */
  private def waitMeasuringElapsed(timeout: Long): Long = if (timeout <= 0) 0 else {
    val start = System.nanoTime()
    wait(timeout)
    val elapsed = System.nanoTime() - start
    // nanoTime should be monotonic, but it's not possible to rely on that.
    // See http://bugs.java.com/bugdatabase/view_bug.do?bug_id=6458294.
    if (elapsed < 0) 0 else TimeUnit.NANOSECONDS.toMillis(elapsed)
  }

  /** Waits for this SyncVar to become defined at least for
   *  `timeout` milliseconds (possibly more), and gets its
   *  value.
   *
   *  @param timeout     the amount of milliseconds to wait, 0 means forever
   *  @return            `None` if variable is undefined after `timeout`, `Some(value)` otherwise
   */
  def get(timeout: Long): Option[A] = synchronized {
    /* Defending against the system clock going backward
     * by counting time elapsed directly.  Loop required
     * to deal with spurious wakeups.
     */
    var rest = timeout
    while (!isDefined && rest > 0) {
      val elapsed = waitMeasuringElapsed(rest)
      rest -= elapsed
    }
    value
  }

  /**
   * Waits for this SyncVar to become defined and returns
   * the result, unsetting the stored value before returning.
   *
   * @return value that was held in this container
   */
  def take(): A = synchronized {
    try get
    finally unsetVal()
  }

  /** Waits for this SyncVar to become defined at least for
   *  `timeout` milliseconds (possibly more), and takes its
   *  value by first reading and then removing the value from
   *  the SyncVar.
   *
   *  @param timeout     the amount of milliseconds to wait, 0 means forever
   *  @return            the value or a throws an exception if the timeout occurs
   *  @throws NoSuchElementException on timeout
   */
  def take(timeout: Long): A = synchronized {
    try get(timeout).get
    finally unsetVal()
  }

  // TODO: this method should be private
  // [Heather] the reason why: it doesn't take into consideration
  // whether or not the SyncVar is already defined. So, set has been
  // deprecated in order to eventually be able to make "setting" private
  @deprecated("Use `put` instead, as `set` is potentially error-prone", "2.10.0")
  // NOTE: Used by SBT 0.13.0-M2 and below
  def set(x: A): Unit = setVal(x)

  /** Places a value in the SyncVar. If the SyncVar already has a stored value,
   * it waits until another thread takes it */
  def put(x: A): Unit = synchronized {
    while (isDefined) wait()
    setVal(x)
  }

  /** Checks whether a value is stored in the synchronized variable */
  def isSet: Boolean = synchronized {
    isDefined
  }

  // TODO: this method should be private
  // [Heather] the reason why: it doesn't take into consideration
  // whether or not the SyncVar is already defined. So, unset has been
  // deprecated in order to eventually be able to make "unsetting" private
  @deprecated("Use `take` instead, as `unset` is potentially error-prone", "2.10.0")
  // NOTE: Used by SBT 0.13.0-M2 and below
  def unset(): Unit = synchronized {
    isDefined = false
    value = None
    notifyAll()
  }

  // `setVal` exists so as to retroactively deprecate `set` without
  // deprecation warnings where we use `set` internally. The
  // implementation of `set` was moved to `setVal` to achieve this
  private def setVal(x: A): Unit = synchronized {
    isDefined = true
    value = Some(x)
    notifyAll()
  }

  // `unsetVal` exists so as to retroactively deprecate `unset` without
  // deprecation warnings where we use `unset` internally. The
  // implementation of `unset` was moved to `unsetVal` to achieve this
  private def unsetVal(): Unit = synchronized {
    isDefined = false
    value = None
    notifyAll()
  }

}
