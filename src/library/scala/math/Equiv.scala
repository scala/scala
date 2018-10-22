/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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

  /**
   * @deprecated since 2.13.0. This implicit universal `Equiv` instance allows accidentally
   * comparing instances of types for which equality isn't well-defined or implemented.
   * (For example, it does not make sense to compare two `Function1` instances.)
   *
   * Use `Equiv.universal` explicitly instead. If you really want an implicit univeral `Equiv` instance
   * despite the potential problems, consider `implicit def universalEquiv[T]: Equiv[T] = universal[T]`.
   */
  @deprecated("Use explicit Equiv.universal instead. See Scaladoc entry for more information: " +
    "https://www.scala-lang.org/api/2.13.0/scala/math/Equiv$.html#universalEquiv[T]:scala.math.Equiv[T]",
    since = "2.13.0")
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

  implicit def equivFromPartialOrdering[T](implicit ev: PartialOrdering[T]): Equiv[T] = ev
}
