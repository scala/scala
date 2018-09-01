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

/** A trait for representing equivalence relations.  It is important to
 *  distinguish between a type that can be compared for equality or
 *  equivalence and a representation of equivalence on some type. This
 *  trait is for representing the latter.
 *
 *  An [[http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]
 *  is a binary relation on a type. This relation is exposed as
 *  the `equiv` method of the `Equiv` trait.  The relation must be:
 *
 *    1. reflexive: `equiv(x, x) == true` for any x of type `T`.
 *    1. symmetric: `equiv(x, y) == equiv(y, x)` for any `x` and `y` of type `T`.
 *    1. transitive: if `equiv(x, y) == true` and `equiv(y, z) == true`, then
 *       `equiv(x, z) == true` for any `x`, `y`, and `z` of type `T`.
 *
 *  @author  Geoffrey Washburn, Paul Phillips
 *  @since 2.7
 */

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
  def reference[T <: AnyRef]: Equiv[T] = { _ eq _ }
  def universal[T]: Equiv[T] = { _ == _ }
  def fromComparator[T](cmp: Comparator[T]): Equiv[T] = {
    (x, y) => cmp.compare(x, y) == 0
  }
  def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = {
    (x, y) => cmp(x, y)
  }
  def by[T, S: Equiv](f: T => S): Equiv[T] =
    ((x, y) => implicitly[Equiv[S]].equiv(f(x), f(y)))

  @inline def apply[T: Equiv]: Equiv[T] = implicitly[Equiv[T]]
}
