/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

/** A trait for representing partial orderings.  It is important to
 *  distinguish between a type that has a partial order and a representation
 *  of partial ordering on some type.  This trait is for representing the
 *  latter.
 *
 *  A [[https://en.wikipedia.org/wiki/Partially_ordered_set partial ordering]] is a
 *  binary relation on a type `T`, exposed as the `lteq` method of this trait.
 *  This relation must be:
 *
 *  - reflexive: `lteq(x, x) == '''true'''`, for any `x` of type `T`.
 *  - anti-symmetric: if `lteq(x, y) == '''true'''` and
 *    `lteq(y, x) == '''true'''`
 *    then `equiv(x, y) == '''true'''`, for any `x` and `y` of type `T`.
 *  - transitive: if `lteq(x, y) == '''true'''` and
 *    `lteq(y, z) == '''true'''` then `lteq(x, z) == '''true'''`,
 *    for any `x`, `y`, and `z` of type `T`.
 *
 *  Additionally, a partial ordering induces an
 *  [[https://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]
 *  on a type `T`: `x` and `y` of type `T` are equivalent if and only if
 *  `lteq(x, y) && lteq(y, x) == '''true'''`. This equivalence relation is
 *  exposed as the `equiv` method, inherited from the
 *  [[scala.math.Equiv Equiv]] trait.
 */

trait PartialOrdering[T] extends Equiv[T] {
  outer =>

  /** Result of comparing `x` with operand `y`.
   *  Returns `None` if operands are not comparable.
   *  If operands are comparable, returns `Some(r)` where
   *  - `r < 0`    iff    `x < y`
   *  - `r == 0`   iff    `x == y`
   *  - `r > 0`    iff    `x > y`
   */
  def tryCompare(x: T, y: T): Option[Int]

  /** Returns `'''true'''` iff `x` comes before `y` in the ordering.
   */
  def lteq(x: T, y: T): Boolean

  /** Returns `'''true'''` iff `y` comes before `x` in the ordering.
   */
  def gteq(x: T, y: T): Boolean = lteq(y, x)

  /** Returns `'''true'''` iff `x` comes before `y` in the ordering
   *  and is not the same as `y`.
   */
  def lt(x: T, y: T): Boolean = lteq(x, y) && !equiv(x, y)

  /** Returns `'''true'''` iff `y` comes before `x` in the ordering
   *  and is not the same as `x`.
   */
  def gt(x: T, y: T): Boolean = gteq(x, y) && !equiv(x, y)

  /** Returns `'''true'''` iff `x` is equivalent to `y` in the ordering.
   */
  def equiv(x: T, y: T): Boolean = lteq(x,y) && lteq(y,x)

  def reverse : PartialOrdering[T] = new PartialOrdering[T] {
    override def reverse = outer
    def tryCompare(x: T, y: T) = outer.tryCompare(y, x)
    def lteq(x: T, y: T) = outer.lteq(y, x)
    override def gteq(x: T, y: T) = outer.gteq(y, x)
    override def lt(x: T, y: T) = outer.lt(y, x)
    override def gt(x: T, y: T) = outer.gt(y, x)
    override def equiv(x: T, y: T) = outer.equiv(y, x)
  }
}

object PartialOrdering {
  @inline def apply[T](implicit ev: PartialOrdering[T]): PartialOrdering[T] = ev
}
