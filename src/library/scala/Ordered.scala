/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** A class for totally ordered data.
 *
 *  Note that since version 2006-07-24 this class is no longer covariant in a.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2006-07-24
 */
trait Ordered[a] {

  /** Result of comparing <code>this</code> with operand <code>that</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def compare(that: a): Int

  def <  (that: a): Boolean = (this compare that) <  0
  def >  (that: a): Boolean = (this compare that) >  0
  def <= (that: a): Boolean = (this compare that) <= 0
  def >= (that: a): Boolean = (this compare that) >= 0
  def compareTo(that: a): Int = compare(that)
}
