/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

/** A class to provide safe concurrent access to a mutable cell.
 *  All methods are synchronized.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class SyncVar[A] {
  private var isDefined: Boolean = false
  private var value: A = _

  def get = synchronized {
    while (!isDefined) wait()
    value
  }

  /** Waits `timeout` millis. If `timeout <= 0` just returns 0. If the system clock
   *  went backward, it will return 0, so it never returns negative results.
   */
  private def waitMeasuringElapsed(timeout: Long): Long = if (timeout <= 0) 0 else {
    val start = System.currentTimeMillis
    wait(timeout)
    val elapsed = System.currentTimeMillis - start
    if (elapsed < 0) 0 else elapsed
  }

  /** Waits for this SyncVar to become defined at least for
   *  `timeout` milliseconds (possibly more), and gets its
   *  value.
   *
   *  @param timeout     the amount of milliseconds to wait, 0 means forever
   *  @return            `None` if variable is undefined after `timeout`, `Some(value)` otherwise
   */
  def get(timeout: Long): Option[A] = synchronized {
    /** Defending against the system clock going backward
     *  by counting time elapsed directly.  Loop required
     *  to deal with spurious wakeups.
     */
    var rest = timeout
    while (!isDefined && rest > 0) {
      val elapsed = waitMeasuringElapsed(rest)
      rest -= elapsed
    }
    if (isDefined) Some(value)
    else None
  }

  def take() = synchronized {
    try get
    finally unset()
  }

  def set(x: A) = synchronized {
    value = x
    isDefined = true
    notifyAll()
  }

  def put(x: A) = synchronized {
    while (isDefined) wait()
    set(x)
  }

  def isSet: Boolean = synchronized {
    isDefined
  }

  def unset(): Unit = synchronized {
    isDefined = false
    notifyAll()
  }
}

