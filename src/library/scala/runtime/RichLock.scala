/*                     __                                               *\
 * *     ________ ___   / /  ___     Scala API                            **
 * *    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
 * *  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 * * /____/\___/_/ |_/____/_/ | |                                         **
 * *                          |/                                          **
\*                                                                      */

package scala
package runtime

import java.util.concurrent.locks.Lock

/**
  * @author Luke Cycon <luke@lukecycon.com>
  */
final class RichLock(val self: Lock) extends AnyRef {

  /**
    *   Run `body` after acquiring this lock, and then releasing it after
    */
  def doWith[T](body: => T): T = {
    self.lock()
    try {
      body
    } finally {
      self.unlock()
    }
  }

}
