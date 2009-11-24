/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

import generic._
import mutable.{Builder, AddingBuilder}
import PartialFunction._

/** <p>
 *    A generic template for sets of type <code>A</code>.<br/>
 *    To implement a concrete set, you need to provide implementations of the
 *    following methods (where <code>This</code> is the type of the set in
 *    question):
 *  </p>
 *  <pre>
 *    <b>def</b> contains(key: A): Boolean
 *    <b>def</b> iterator: Iterator[A]
 *    <b>def</b> +(elem: A): This
 *    <b>def</b> -(elem: A): This</pre>
 *  <p>
 *    If you wish that methods <code>like</code>, <code>take</code>, <code>drop</code>,
 *    <code>filter</code> return the same kind of set, you should also override:
 *  </p>
 *  <pre>
 *   <b>def</b> empty: This</pre>
 *  <p>
 *    It is also good idea to override methods <code>foreach</code> and
 *    <code>size</code> for efficiency.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait SetLike[A, +This <: SetLike[A, This] with Set[A]] extends IterableLike[A, This] with Addable[A, This] with Subtractable[A, This] {
self =>

  /* The empty set of the dame type as this set */
  def empty: This

  /** A common implementation of <code>newBuilder</code> for all sets in terms
   *  of <code>empty</code>. Overridden for mutable sets in
   *  <a href="mutable/SetLike.html" target="ContentFrame">
   *  <code>mutable.SetLike</code></a>.
   */
  override protected[this] def newBuilder: Builder[A, This] = new AddingBuilder[A, This](empty)

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param elem the element to check for membership.
   *  @return     <code>true</code> iff <code>elem</code> is contained in
   *              this set.
   */
  def contains(elem: A): Boolean

  /** Creates a new set with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   */
  def + (elem: A): This

  /** Creates a new set with given element removed from this set, unless the
   *  element is not present.
   *
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

  /** Returns a new set consisting of all elements that are both in the current
   *  set and in the argument set.
   *
   *  @param that the set to intersect with.
   */
  def intersect(that: Set[A]): This = filter(that.contains)

  /** Returns a new set consisting of all elements that are both in the current
   *  set and in the argument set.
   *
   *  @param that the set to intersect with.
   *  @note  same as <code>intersect</code>.
   */
  def &(that: Set[A]): This = intersect(that)

 /**  This method is an alias for <code>intersect</code>.
   *  It computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   */
  @deprecated("use & instead") def ** (that: Set[A]): This = intersect(that)

  /** The union of this set and the given set <code>that</code>.
   *
   *  @param that the set of elements to add
   *  @return     a set containing the elements of this
   *              set and those of the given set <code>that</code>.
   */
  def union(that: Set[A]): This = this.++(that)

  /** The union of this set and the given set <code>that</code>.
   *
   *  @param that the set of elements to add
   *  @return     a set containing the elements of this
   *              set and those of the given set <code>that</code>.
   *  @note       same as <code>union</code>.
   */
  def | (that: Set[A]): This = union(that)

  /** The difference of this set and the given set <code>that</code>.
   *
   *  @param that the set of elements to remove
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set <code>that</code>.
   */
  def diff(that: Set[A]): This = --(that)

  /** The difference of this set and the given set <code>that</code>.
   *
   *  @param that the set of elements to remove
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set <code>that</code>.
   *  @note       same as <code>diff</code>.
   */
  def &~(that: Set[A]): This = diff(that)

  /** Checks if this set is a subset of set <code>that</code>.
   *
   *  @param that another set.
   *  @return     <code>true</code> iff the other set is a superset of
   *              this set.
   *  todo: rename to isSubsetOf
   */
  def subsetOf(that: Set[A]): Boolean = forall(that.contains)

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override def stringPrefix: String = "Set"

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableLike].toString

  override def hashCode() = this map (_.hashCode) sum

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
    case that: Set[_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) &&
      (try this subsetOf that.asInstanceOf[Set[A]]
       catch { case ex: ClassCastException => false })
    case _ =>
      false
  }
}
