/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

/** A default map which implements the `+` and `-` methods of maps.
 *
 *  Instances that inherit from `DefaultMap[A, B]` still have to define:
 *  {{{
 *    def get(key: A): Option[B]
 *    def iterator: Iterator[(A, B)]
 *  }}}
 *  It refers back to the original map.
 *
 *  It might also be advisable to override `foreach` or `size` if efficient
 *  implementations can be found.
 *
 *  @since 2.8
 */
trait DefaultMap[A, +B] extends Map[A, B] { self =>

  /** A default implementation which creates a new immutable map.
   */
  override def +[B1 >: B](kv: (A, B1)): Map[A, B1] = {
    val b = Map.newBuilder[A, B1]
    b ++= this
    b += ((kv._1, kv._2))
    b.result()
  }

  /** A default implementation which creates a new immutable map.
   */
  override def - (key: A): Map[A, B] = {
    val b = newBuilder
    b ++= this filter (key != _._1)
    b.result()
  }
}
