/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.math

/** A class for partially ordered data.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 23/04/2004
 */
trait PartiallyOrdered[+A] {

  /** Result of comparing <code>this</code> with operand <code>that</code>.
   *  Returns <code>None</code> if operands are not comparable.
   *  If operands are comparable, returns <code>Some(x)</code> where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def tryCompareTo [B >: A <% PartiallyOrdered[B]](that: B): Option[Int]

  def <  [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x < 0 => true
      case _ => false
    }
  def >  [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x > 0 => true
      case _ => false
    }
  def <= [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x <= 0 => true
      case _ => false
    }
  def >= [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x >= 0 => true
      case _ => false
    }
}
