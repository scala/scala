/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.math

/** <p>
 *    A trait for representing partial orderings.  It is important to
 *    distinguish between a type that has a partial order and a representation
 *    of partial ordering on some type.  This trait is for representing the
 *    latter.
 *  </p>
 *  <p>
 *    A <a href="http://en.wikipedia.org/wiki/Partial_order">partial ordering</a>
 *    is a binary relation on a type <code>T</code> that is also an equivalence
 *    relation on values of type <code>T</code>.  This relation is exposed as
 *    the <code>lteq</code> method of the <code>PartialOrdering</code> trait.
 *    This relation must be:
 *  </p>
 *  <ul>
 *    <li>reflexive: <code>lteq(x, x) == true</code>, for any <code>x</code> of
 *      type <code>T</code>.</li>
 *    <li>anti-symmetric: <code>lteq(x, y) == true</code> and
 *      <code>lteq(y, x) == true</code> then <code>equiv(x, y)</code>, for any
 *      <code>x</code> and <code>y</code> of type <code>T</code>.</li>
 *    <li>transitive: if <code>lteq(x, y) == true</code> and
 *      <code>lteq(y, z) == true</code> then <code>lteq(x, z) == true</code>,
 *      for any <code>x</code>, <code>y</code>, and <code>z</code> of type
 *      <code>T</code>.</li>
 * </ul>
 *
 *  @author  Geoffrey Washburn
 *  @version 1.0, 2008-04-0-3
 *  @since 2.7
 */

trait PartialOrdering[T] extends Equiv[T] {
  outer =>

  /** Result of comparing <code>x</code> with operand <code>y</code>.
   *  Returns <code>None</code> if operands are not comparable.
   *  If operands are comparable, returns <code>Some(r)</code> where
   *  <code>r &lt; 0</code>    iff    <code>x &lt; y</code>
   *  <code>r == 0</code>   iff    <code>x == y</code>
   *  <code>r &gt; 0</code>    iff    <code>x &gt; y</code>
   */
  def tryCompare(x: T, y: T): Option[Int]

  /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering.
   */
  def lteq(x: T, y: T): Boolean

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering.
   */
  def gteq(x: T, y: T): Boolean = lteq(y, x)

  /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering and is not the same as <code>y</code>.
   */
  def lt(x: T, y: T): Boolean = lteq(x, y) && !equiv(x, y)

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering and is not the same as <code>x</code>.
   */
  def gt(x: T, y: T): Boolean = gteq(x, y) && !equiv(x, y)

  /** Returns <code>true</code> iff <code>x</code> is equivalent to
   *  <code>y</code> in the ordering.
   */
  def equiv(x: T, y: T): Boolean = lteq(x,y) && lteq(y,x)

  def reverse : PartialOrdering[T] = new PartialOrdering[T] {
    override def reverse = outer
    def lteq(x: T, y: T) = outer.lteq(y, x)
    def tryCompare(x: T, y: T) = outer.tryCompare(y, x)
  }
}
