/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package math

import java.util.Comparator

/** Defines an equivalence relations on some type `T`.
 *
 *  An [[http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]
 *  is a binary relation on a type `T`, exposed as
 *  the `equiv` method of this trait, that must be:
 *
 *    1. reflexive: `x = x`
 *    1. symmetric: if `x = y` then `y = x`
 *    1. transitive: if `x = y` and `y = z`, then `x = z`
 *
 *  @author  Geoffrey Washburn, Paul Phillips
 *  @version 1.0, 2008-04-03
 *  @since 2.7
 */
@annotation.implicitNotFound(msg = "No implicit Equiv defined for ${T}.")
trait Equiv[T] extends Any with Serializable {
  /** Returns `true` iff `x` is equivalent to `y`.
   */
  def equiv(x: T, y: T): Boolean
}

trait LowPriorityEquiv {
  self: Equiv.type =>

  implicit def universalEquiv[T] : Equiv[T] = universal[T]
}

object Equiv extends LowPriorityEquiv {
  def reference[T <: AnyRef]: Equiv[T] = new Equiv[T] {
    def equiv(x: T, y: T) = x eq y
  }
  def universal[T]: Equiv[T] = new Equiv[T] {
    def equiv(x: T, y: T) = x == y
  }
  def fromComparator[T](cmp: Comparator[T]): Equiv[T] = new Equiv[T] {
    def equiv(x: T, y: T) = cmp.compare(x, y) == 0
  }
  def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = new Equiv[T] {
    def equiv(x: T, y: T) = cmp(x, y)
  }
  def by[T, S: Equiv](f: T => S): Equiv[T] =
    fromFunction((x, y) => implicitly[Equiv[S]].equiv(f(x), f(y)))

  @inline def apply[T: Equiv]: Equiv[T] = implicitly[Equiv[T]]
}
