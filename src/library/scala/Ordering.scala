/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/** A trait for representing total orderings.  It is important to
 * distinguish between a type that has a total order and a represnetation
 * of total  ordering on some type.  This trait is for representing the
 * latter.
 *
 * A <a href="http://en.wikipedia.org/wiki/Total_order">total ordering</a>
 * is a binary relation on a type <code>T</code> that is also an equivalence relation
 * and partial ordering on values of type <code>T</code>.  This relation is exposed as
 * the <code>compare</code> method of the <code>Ordering</code> trait.
 * This relation must be:
 * <ul>
 * <li>reflexive: <code>compare(x, x) == 0</code>, for any <code>x</code> of
 * type <code>T</code>.</li>
 * <li>(need a name): <code>compare(x, y) == z</code> and <code>compare(y, x) == w</code>
 * then <code>Math.signum(z) == -Math.signum(w)</code>, for any <code>x</code> and <code>y</code> of
 * type <code>T</code> and <code>z</code> and <code>w</code> of type <code>Int</code>.</li>
 * <li>transitive: if <code>compare(x, y) == z</code> and <code>lteq(y, w) == v</code>
 * and <code>Math.signum(z) &gt;= 0</code> and <code>Math.signum(v) &gt;= 0</code> then
 * <code>compare(x, w) == u</code> and <code>Math.signum(z + v) == Math.signum(u)</code>,
 * for any <code>x</code>, <code>y</code>,
 * and <code>w</code> of type <code>T</code> and <code>z</code>, <code>v</code>, and <code>u</code>
 * of type <code>Int</code>.</li>
 * </ul>
 *
 * @author Geoffrey Washburn
 * @version 0.9, 2008-04-03
 */

trait Ordering[T] extends PartialOrdering[T] {
 /** Returns a negative integer iff <code>x</code> comes before
   * <code>y</code> in the ordering, returns 0 iff <code>x</code>
   * is the same in the ordering as <code>y</code>, and returns a
   * positive number iff <code>x</code> comes after
   * <code>y</code> in the ordering.
   */
  def compare(x: T, y: T): Int

 /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering.
   */
  override def lteq(x: T, y: T): Boolean = compare(x, y) <= 0

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering.
   */
  override def gteq(x: T, y: T): Boolean = compare(x, y) >= 0

  /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering and is not the same as <code>y</code>.
   */
  override def lt(x: T, y: T): Boolean = compare(x, y) < 0

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering and is not the same as <code>x</code>.
   */
  override def gt(x: T, y: T): Boolean = compare(x, y) > 0

  /** Returns <code>true</code> iff <code>x</code> is equivalent to
   *  <code>y</code> in the ordering.
   */
  override def equiv(x: T, y: T): Boolean = compare(x, y) == 0
}
