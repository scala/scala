/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-04, LAMP/EPFL               **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** A trait for totally ordered data.
 */
trait Ordered[+a] {

  /** Result of comparing `this' with operand `that'.
   *  returns `x' where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def compareTo [b >: a <% Ordered[b]](that: b): int;

  def <  [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) <  0;

  def >  [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) >  0;

  def <= [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) <= 0;

  def >= [b >: a <% Ordered[b]](that: b): boolean = (this compareTo that) >= 0;
}
