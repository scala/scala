/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


/** The class <code>SyncVar</code> ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class SyncVar[a] {
  private var isDefined: Boolean = false
  private var value: a = _

  def get = synchronized {
    if (!isDefined) wait()
    value
  }

  def set(x: a) = synchronized {
    value = x
    isDefined = true
    notifyAll()
  }

  def isSet: Boolean = synchronized {
    isDefined
  }

  def unset = synchronized {
    isDefined = false
  }

}
