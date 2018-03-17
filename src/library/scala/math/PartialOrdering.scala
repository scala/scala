/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package math

/** Defines a partial ordering on some type `T`.
 *
 *  A [[http://en.wikipedia.org/wiki/Partially_ordered_set partial ordering]] is a
 *  binary relation on a type `T`, exposed as the `lteq` method of this trait,
 *  that must be:
 *
 *    1. reflexive: `x <= x`
 *    1. antisymmetric: if `x <= y` and `y <= x` then `x = y`
 *    1. transitive: if `x <= y` and `y <= z` then `x <= z`
 *
 *  Additionally, a partial ordering induces an
 *  [[http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]
 *  on a type `T`,
 *  exposed as the `equiv` method, inherited from the
 *  [[scala.math.Equiv Equiv]] trait.
 *
 *  @author  Geoffrey Washburn
 *  @version 1.0, 2008-04-0-3
 *  @since 2.7
 */
@annotation.implicitNotFound(msg = "No implicit PartialOrdering defined for ${T}.")
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
    def lteq(x: T, y: T) = outer.lteq(y, x)
    def tryCompare(x: T, y: T) = outer.tryCompare(y, x)
  }
}
