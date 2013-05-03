/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.hashing






/** A fast multiplicative hash by Phil Bagwell.
 */
final class ByteswapHashing[T] extends Hashing[T] {

  def hash(v: T) = byteswap32(v.##)

}


object ByteswapHashing {

  private class Chained[T](h: Hashing[T]) extends Hashing[T] {
    def hash(v: T) = byteswap32(h.hash(v))
  }

  /** Composes another `Hashing` with the Byteswap hash.
   */
  def chain[T](h: Hashing[T]): Hashing[T] = new Chained(h)

}
