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
  private var value: Option[A] = None

  def get: A = synchronized {
    while (!isDefined) wait()
    value.get
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
    value
  }

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
   *  @return            `None` if variable is undefined after `timeout`, `Some(value)` otherwise
   */
  def take(timeout: Long): A = synchronized {
    try get(timeout).get
    finally unsetVal()
  }

  // TODO: this method should be private
  // [Heather] the reason why: it doesn't take into consideration 
  // whether or not the SyncVar is already defined. So, set has been
  // deprecated in order to eventually be able to make "setting" private
  @deprecated("Use `put` instead, as `set` is potentionally error-prone", "2.10.0")
  def set(x: A): Unit = setVal(x)

  def put(x: A): Unit = synchronized {
    while (isDefined) wait()
    setVal(x)
  }

  def isSet: Boolean = synchronized {
    isDefined
  }

  // TODO: this method should be private
  // [Heather] the reason why: it doesn't take into consideration 
  // whether or not the SyncVar is already defined. So, unset has been
  // deprecated in order to eventually be able to make "unsetting" private  
  @deprecated("Use `take` instead, as `unset` is potentionally error-prone", "2.10.0")
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

