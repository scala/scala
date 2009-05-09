/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scala.collection.generic

/** A generic template for sets of type A.
 *  To implement a concrete set, you need to provide implementations of the following methods:
 *  (where This is the type of the set in question):
 *
 *   def contains(key: A): Boolean
 *   def elements: Iterator[A]
 *   def +(elem: A): This
 *   def -(elem: A): This
 *
 * If you wish that methods like, take, drop, filter return the same kind of set, you should also
 * override:
 *
 *   def empty: This
 *
 * It is also good idea to override methods foreach and size for efficiency.
 */
trait SetTemplate[A, +This <: SetTemplate[A, This] with Set[A]] extends IterableTemplate[A, This] with Addable[A, This] with Subtractable[A, This] { self =>

  def empty: This

  override protected[this] def newBuilder: Builder[A, This] = new AddingBuilder[A, This](empty)

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def contains(elem: A): Boolean

  /** Creates a new set with an additional element, unless the element is already present.
   *  @param elem the element to be added
   */
  def + (elem: A): This

  /** Removes a single element from a set, unless the element is not present.
   *  @param elem the element to be removed
   */
  def - (elem: A): This

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
  def intersect(that: Set[A]): This = filter(that.contains)

 /** This method is an alias for <code>intersect</code>.
   *  It computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   *  @deprecated use & instead
   */
  @deprecated def ** (that: Set[A]): This = intersect(that)

  /** This method is an alias for <code>intersect</code>.
   *  It computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   */
  def & (that: collection.Set[A]): This = intersect(that)

   /** Computes the union of this set and the given set <code>that</code>.
   *
   *  @param that the sequence of elements to add to the sequence.
   *  @return     a set containing the elements of this
   *              set and those of the given set <code>that</code>.
   */
  def union(that: Set[A]): This = (thisCollection /: that) (_ + _)

  /** This method is an alias for <code>union</code>.
   *  It computes the union of this set and the given set <code>that</code>.
   */
  def | (that: Set[A]): This = union(that)

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
   *  @note not necessarily run-time type safe.
   *  @return     <code>true</code> iff this set and the other set
   *              contain the same elements.
   */
  override def equals(that: Any): Boolean = that match {
    case other: Set[_] =>
      if (this.size == other.size)
        try { // can we find a safer way to do this?
          subsetOf(other.asInstanceOf[Set[A]])
        } catch {
          case ex: ClassCastException => false
        }
      else false
    case _ =>
      false
  }

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override def stringPrefix: String = "Set"

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableTemplate].toString
}



