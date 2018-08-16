/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2017, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package math

/** A class for partially ordered data.
 *
 *  @author  Martin Odersky
 */
trait PartiallyOrdered[+A] extends Any {

  type AsPartiallyOrdered[B] = B => PartiallyOrdered[B]

  /** Result of comparing `'''this'''` with operand `that`.
   *  Returns `None` if operands are not comparable.
   *  If operands are comparable, returns `Some(x)` where
   *  - `x < 0`    iff   `'''this''' &lt; that`
   *  - `x == 0`   iff   `'''this''' == that`
   *  - `x > 0`    iff   `'''this''' &gt; that`
   */
  def tryCompareTo [B >: A: AsPartiallyOrdered](that: B): Option[Int]

  def < [B >: A: AsPartiallyOrdered](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x < 0 => true
      case _ => false
    }

  def > [B >: A: AsPartiallyOrdered](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x > 0 => true
      case _ => false
    }

  def <= [B >: A: AsPartiallyOrdered](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x <= 0 => true
      case _ => false
    }

  def >= [B >: A: AsPartiallyOrdered](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x >= 0 => true
      case _ => false
    }
}
