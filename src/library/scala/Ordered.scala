/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** A class for totally ordered data.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 23/04/2004
 */
mixin class Ordered[+a] {

  /** Result of comparing `this' with operand `that'.
   *  returns `x' where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def compareTo [b >: a <% Ordered[b]](that: b): Int;

  def <  [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) <  0;

  def >  [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) >  0;

  def <= [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) <= 0;

  def >= [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) >= 0;
}
