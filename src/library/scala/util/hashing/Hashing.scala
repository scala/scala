/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.hashing

import scala.annotation.implicitNotFound

/** `Hashing` is a trait whose instances each represent a strategy for hashing
  * instances of a type.
  *
  * `Hashing`'s companion object defines a default hashing strategy for all
  * objects - it calls their `##` method.
  *
  * Note: when using a custom `Hashing`, make sure to use it with the `Equiv`
  * such that if any two objects are equal, then their hash codes must be equal.
  *
  * @since 2.10
  */
@implicitNotFound(msg = "No implicit Hashing defined for ${T}.")
trait Hashing[T] extends Serializable {
  def hash(x: T): Int
}

object Hashing {
  final class Default[T] extends Hashing[T] {
    def hash(x: T) = x.##
  }

  implicit def default[T] = new Default[T]

  def fromFunction[T](f: T => Int) = new Hashing[T] {
    def hash(x: T) = f(x)
  }
}
