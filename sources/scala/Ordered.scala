package scala;

/** A trait for totally ordered data.
 */
trait Ordered[+a] {

  /** Result of comparing `this' with operand `that'.
   *  returns `x' where
   *  x < 0    iff    this < that
   *  x == 0   iff    this == that
   *  x > 0    iff    this > that
   */
  def compareTo [b >: a <% Ordered[b]](that: b): int;

  def <  [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) <  0;

  def >  [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) >  0;

  def <= [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) <= 0;

  def >= [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) >= 0;
}
