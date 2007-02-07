/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** A class for partially ordered data.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 23/04/2004
 */
trait PartiallyOrdered[+a] {

  /** Result of comparing <code>this</code> with operand <code>that</code>.
   *  Returns <code>None</code> if operands are not comparable.
   *  If operands are comparable, returns <code>Some(x)</code> where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def tryCompareTo [b >: a <% PartiallyOrdered[b]](that: b): Option[int]

  def <  [b >: a <% PartiallyOrdered[b]](that: b): boolean =
    (this tryCompareTo that) match {
      case Some(x) if x < 0 => true
      case _ => false
    }
  def >  [b >: a <% PartiallyOrdered[b]](that: b): boolean =
    (this tryCompareTo that) match {
      case Some(x) if x > 0 => true
      case _ => false
    }
  def <= [b >: a <% PartiallyOrdered[b]](that: b): boolean =
    (this tryCompareTo that) match {
      case Some(x) if x <= 0 => true
      case _ => false
    }
  def >= [b >: a <% PartiallyOrdered[b]](that: b): boolean =
    (this tryCompareTo that) match {
      case Some(x) if x >= 0 => true
      case _ => false
    }
}
