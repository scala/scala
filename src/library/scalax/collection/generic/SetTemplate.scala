/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Set.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.generic


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
 *  @version 2.8
 */
trait SetTemplate[+CC[B] <: SetTemplate[CC, B] with Set[B], A]
  extends (A => Boolean)
     with SizedIterable[A]
     with IterableTemplate[CC, A]
     with Addable[CC[A], A]
     with Subtractable[CC[A], A] {

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  def size: Int

  /** Creates a new set of this kind with given elements */
  def newBuilder[B]: Builder[CC, B]

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def contains(elem: A): Boolean

  /** Create a new set with an additional element.
   */
  def + (elem: A): CC[A]

  /** Remove a single element from a set.
   *  @param elem the element to be removed
   *  @return a new set with the element removed.
   */
  def - (elem: A): CC[A]

  /** Checks if this set is empty.
   *
   *  @return <code>true</code> iff there is no element in the set.
   */
  override def isEmpty: Boolean = size == 0

  /** This method allows sets to be interpreted as predicates.
   *  It returns <code>true</code>, iff this set contains element
   *  <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def apply(elem: A): Boolean = contains(elem)

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with.
   */
  def intersect(that: Set[A]): CC[A] = filter(that.contains)

  /** This method is an alias for <code>intersect</code>.
   *  It computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   */
  def ** (that: collection.Set[A]): CC[A] = intersect(that)

  /** Checks if this set is a subset of set <code>that</code>.
   *
   *  @param that another set.
   *  @return     <code>true</code> iff the other set is a superset of
   *              this set.
   *  todo: rename to isSubsetOf
   */
  def subsetOf(that: Set[A]): Boolean = forall(that.contains)

/* What a mess! We need to remove these methods, but can't without breaking
 * existing code. What to do?

  /** Compares this set with another object and returns true, iff the
   *  other object is also a set which contains the same elements as
   *  this set.
   *
   *  @param that the other object
   *  @note not necessarily run-time type safe.
   *  @return     <code>true</code> iff this set and the other set
   *              contain the same elements.
   *  @deprecated equals is not stable for mutable sets.
   *              If you wish object identity, use eq or cast this set of AnyRef and then use
   *              equals. If you wish element comparisons, use `sameElements` instead.
   */
  @deprecated override def equals(that: Any): Boolean = that match {
    case other: Set[_] =>
      this.size == other.size && subsetOf(other.asInstanceOf[Set[A]])
    case _ =>
      false
  }

  /*  @deprecated Since the previous hashCode is not stable for mutable sets,
   *              the implementation of this method has been changed to
   *              standard address-based hashCode from java.lang.Object.
   *              If you relied on the old behavior, y
   *              IT has been
   *  if you intend to have object identity hashCode and wish the deprecated warning
   *              to go away, cast this set to AnyRef before calling hashCode.
   */
  @deprecated override def hashCode() =
    (0 /: this)((hash, e) => hash + e.hashCode())
*/
  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override def stringPrefix: String = "Set"

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableTemplate].toString
}
