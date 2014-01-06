/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package math

/** A class for partially ordered data.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 23/04/2004
 */
trait PartiallyOrdered[+A] {

  /** Result of comparing `'''this'''` with operand `that`.
   *  Returns `None` if operands are not comparable.
   *  If operands are comparable, returns `Some(x)` where
   *  - `x < 0`    iff   `'''this''' &lt; that`
   *  - `x == 0`   iff   `'''this''' == that`
   *  - `x > 0`    iff   `'''this''' &gt; that`
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
