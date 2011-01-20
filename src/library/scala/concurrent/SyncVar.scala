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

  def get(timeout: Long): Option[A] = synchronized {
    if (!isDefined) {
      try wait(timeout)
      catch { case _: InterruptedException => () }
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

