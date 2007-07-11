/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


/** The class <code>SyncVar</code> ...
 *
 *  @author  Martin Odersky, Stepan Koltsov
 *  @version 1.0, 10/03/2003
 */
class SyncVar[A] {
  private var isDefined: Boolean = false
  private var value: A = _
  private var exception: Option[Throwable] = None

  def get = synchronized {
    while (!isDefined) wait()
    if (exception.isEmpty) value
    else throw exception.get
  }

  def set(x: A) = synchronized {
    value = x
    isDefined = true
    exception = None
    notifyAll()
  }

  private def setException(e: Throwable) = synchronized {
    exception = Some(e)
    isDefined = true
    notifyAll()
  }

  def setWithCatch(x: => A) = synchronized {
    try {
      this set x
    } catch {
      case e =>
        this setException e
        throw e
    }
  }

  def isSet: Boolean = synchronized {
    isDefined
  }

  def unset = synchronized {
    isDefined = false
  }

}
