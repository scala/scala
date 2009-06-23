/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** A trait for totally ordered data.
 *
 * Note that since version 2006-07-24 this trait is no longer covariant in a.
 *
 * It is important that the equals method for an instance of
 * Ordered[A] be consistent with the compare method.  However,
 * due to limitations inherent in the type erasure semantics,
 * there is no reasonable way to provide a default implementation
 * of equality for instances of Ordered[A].  Therefore, if you need
 * to be able to use equality on an instance of Ordered[A] you must
 * provide it yourself either when inheiriting or instantiating.
 *
 * It is important that the hashCode method for an instance of
 * Ordered[A] be consistent with the compare method. However,
 * it is not possible to provide a sensible default implementation.
 * Therefore, if you need to be able compute the hash of an
 * instance of Ordered[A] you must provide it yourself either when
 * inheiriting or instantiating.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2006-07-24
 */
trait Ordered[A] extends java.lang.Comparable[A] {

  /** Result of comparing <code>this</code> with operand <code>that</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this &lt; that</code>
   *  <code>x == 0</code>   iff    <code>this == that</code>
   *  <code>x &gt; 0</code>    iff    <code>this &gt; that</code>
   */
  def compare(that: A): Int

  def <  (that: A): Boolean = (this compare that) <  0
  def >  (that: A): Boolean = (this compare that) >  0
  def <= (that: A): Boolean = (this compare that) <= 0
  def >= (that: A): Boolean = (this compare that) >= 0
  def compareTo(that: A): Int = compare(that)


}
