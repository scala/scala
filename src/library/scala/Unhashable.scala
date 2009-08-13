/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>
 *    A marker trait for data structures that cannot be hashed, for instance
 *    because they are mutable and at the same time support structural
 *    equality, so hashing them would lead to unpredictable results.
 *  </p>
 *  <p>
 *    Such data structures have <code>hashCode</code> throw an <a href=""
 *    target="contentFrame" class="java/lang/UnsupportedOperationException">
 *    <code>java.lang.UnsupportedOperationException</code></a>. They retain
 *    the original object hashcode with <code>identityHashCode</code>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.0
 *  @since   2.8
 */
trait Unhashable extends Object {

  /** The hashCode method always yields an error, since it is not
   *  safe to use sets as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode: Int =
    throw new UnsupportedOperationException("unsuitable as hash key")

  /** The identity hash code is the original hash code inherited from Object.
   */
  def identityHashCode: Int =
    super/*[Object]*/.hashCode  // !!! super[Object] does not work here
}
