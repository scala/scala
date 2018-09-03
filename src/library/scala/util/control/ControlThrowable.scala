/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

/** A marker trait indicating that the `Throwable` it is mixed into is
 *  intended for flow control.
 *
 *  Note that `Throwable` subclasses which extend this trait may extend any
 *  other `Throwable` subclass (eg. `RuntimeException`) and are not required
 *  to extend `Throwable` directly.
 *
 *  Instances of `Throwable` subclasses marked in this way should not normally
 *  be caught. Where catch-all behaviour is required `ControlThrowable`
 *  should be propagated, for example:
 *  {{{
 *  import scala.util.control.ControlThrowable
 *
 *  try {
 *    // Body might throw arbitrarily
 *  } catch {
 *    case c: ControlThrowable => throw c // propagate
 *    case t: Exception        => log(t)  // log and suppress
 *  }
 *  }}}
 *
 *  @author Miles Sabin
 */
trait ControlThrowable extends Throwable with NoStackTrace

object ControlThrowable {
  sealed trait CacheableBase extends ControlThrowable {
    override def fillInStackTrace(): Throwable = this
  }

  class Cacheable(message: String)
    extends Throwable(message, null, false, false)
    with CacheableBase {
    def this() = this(null)
  }

  class CacheableError(message: String)
    extends Error(message, null, false, false)
    with CacheableBase {
    def this() = this(null)
  }

  class CacheableException(message: String)
    extends Exception(message, null, false, false)
    with CacheableBase {
    def this() = this(null)
  }

  class CacheableRuntimeException(message: String)
    extends RuntimeException(message, null, false, false)
    with CacheableBase {
    def this() = this(null)
  }
}
