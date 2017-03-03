/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.concurrent

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
@deprecated("use java.util.concurrent.locks.Lock", "2.11.2")
class Lock {
  var available = true

  def acquire() = synchronized {
    while (!available) wait()
    available = false
  }

  def release() = synchronized {
    available = true
    notify()
  }
}
