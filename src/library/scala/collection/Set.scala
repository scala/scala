/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection


/** <p>
 *    A set is a collection that includes at most one of any object.
 *  </p>
 *  <p>
 *    This trait provides a limited interface, only allowing reading of elements.
 *    There are two extensions of this trait, in packages
 *    <code><a href="mutable$content.html" target="contentFrame">
 *    scala.collection.mutable</a></code>
 *    and <code><a href="immutable$content.html" target="contentFrame">
 *    scala.collection.immutable</a></code>, which provide functionality for
 *    adding and removing objects from the set. The trait in the first package is
 *    for sets that are modified destructively, whereas the trait in
 *    the second package is for immutable sets which create a new set
 *    when something is added or removed to them.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 */
trait Set[A] extends (A => Boolean) with Collection[A] {

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  def size: Int

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def contains(elem: A): Boolean

  /** This method allows sets to be interpreted as predicates.
   *  It returns <code>true</code>, iff this set contains element
   *  <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def apply(elem: A): Boolean = contains(elem)

  /** Checks if this set is empty.
   *
   *  @return <code>true</code> iff there is no element in the set.
   */
  override def isEmpty: Boolean = size == 0

  /** Checks if this set is a subset of set <code>that</code>.
   *
   *  @param that another set.
   *  @return     <code>true</code> iff the other set is a superset of
   *              this set.
   *  todo: rename to isSubsetOf
   */
  def subsetOf(that: Set[A]): Boolean = forall(that.contains)

  /** Compares this set with another object and returns true, iff the
   *  other object is also a set which contains the same elements as
   *  this set.
   *
   *  @param that the other object
   *  @return     <code>true</code> iff this set and the other set
   *              contain the same elements.
   */
  override def equals(that: Any): Boolean = that match {
    case other: Set[a] =>
      this.size == other.size && subsetOf(other.asInstanceOf[Set[A]])
    case _ =>
      false
  }

  /** hashcode for this set */
  override def hashCode() =
    (0 /: this)((hash, e) => hash + e.hashCode())


  override def toArray[B >: A]: Array[B] = {
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }
}
