package scala;

/** A trait for partially ordered data.
 */
trait PartiallyOrdered[+a] {

  /** Result of comparing `this' with operand `that'.
   *  Returns `None' if operands are not comparable.
   *  If operands are comparable, returns `Some(x)' where
   *  x < 0    iff    this < that
   *  x == 0   iff    this == that
   *  x > 0    iff    this > that
   */
  def tryCompareTo [b >: a <% PartiallyOrdered[b]](that: b): Option[int];

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
