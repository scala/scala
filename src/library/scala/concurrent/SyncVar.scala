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
  @volatile private var isDefined: Boolean = false
  @volatile private var value: A = _

  def get = synchronized {
    while (!isDefined) wait()
    value
  }

  /** Like Object.wait but reports millis elapsed.
   */
  private def waitMeasuringElapsed(timeout: Long): Long = {
    val start = System.currentTimeMillis
    wait(timeout)
    System.currentTimeMillis - start
  }

  def get(timeout: Long): Option[A] = synchronized {
    /** Defending against the system clock going backward
     *  by counting time elapsed directly.  Loop required
     *  to deal with spurious wakeups.
     */
  	var rest = timeout
  	while (!isDefined && rest >= 0) {
  	  val elapsed = waitMeasuringElapsed(timeout)
  	  if (!isDefined && elapsed > 0)
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

