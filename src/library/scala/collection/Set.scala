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
 *    This class defines the interface of collections that do not contain
 *    duplicate elements.
 *  </p>
 *  <p>
 *    Class <code>Set</code> may only be used for accessing elements
 *    from set implementations. Two different extensions
 *    of class <code>Set</code> in the package
 *    <code><a href="mutable$content.html" target="contentFrame">
 *    scala.collection.mutable</a></code> and
 *    <code><a href="immutable$content.html" target="contentFrame">
 *    scala.collection.immutable</a></code> provide functionality for adding
 *    new elements to a set. The class in the first package is implemented
 *    by sets the are modified destructively, whereas the class in the second
 *    package is used by functional set implementations that rely on immutable
 *    data structures.
 *  </p>
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 */
trait Set[A] extends (A => Boolean) with Iterable[A] {

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
  def isEmpty: Boolean = size == 0

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
    (0 /: this)((hash, e) => hash * 41 + e.hashCode())

  /** Returns a string representation of this set.
   *
   *  @return a string showing all elements of this set.
   */
  override def toString(): String = mkString("Set(", ", ", ")")

}
