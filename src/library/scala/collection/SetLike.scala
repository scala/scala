/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

import generic._
import mutable.{Builder, AddingBuilder}
import PartialFunction._

/** A template trait for sets of type `Set[A]`.
 *
 * This trait provides most of the operations of a `Set` independently of its representation.
 * It is typically inherited by concrete implementations of sets.
 *
 * $setnote
 *
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @define setnote
 *    To implement a concrete set, you need  to provide implementations of the
 *    following methods:
 *    {{{
 *       def contains(key: A): Boolean
 *       def iterator: Iterator[A]
 *       def +(elem: A): This
 *       def -(elem: A): This
 *    }}}
 *    If you wish that methods like `take`, `drop`,
 *    `filter` return the same kind of set, you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 *   @define coll set
 *   @define Coll Set
 *   @define willNotTerminateInf
 *   @define mayNotTerminateInf
 */
trait SetLike[A, +This <: SetLike[A, This] with Set[A]]
extends IterableLike[A, This]
   with Addable[A, This]
   with Subtractable[A, This] {
self =>

  /** The empty set of the same type as this set
   * @return  an empty set of type `This`.
   */
  def empty: This

  /** A common implementation of `newBuilder` for all sets in terms
   *  of `empty`. Overridden for mutable sets in
   *  <a href="mutable/SetLike.html" target="ContentFrame">
   *  `mutable.SetLike`</a>.
   */
  override protected[this] def newBuilder: Builder[A, This] = new AddingBuilder[A, This](empty)

  /** Tests if some element is contained in this set.
   *
   *  @param elem the element to test for membership.
   *  @return     `true` if `elem` is contained in this set, `false` otherwise.
   */
  def contains(elem: A): Boolean

  /** Creates a new set with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new set that contains all elements of this set and that also
   *          contains `elem`.
   */
  def + (elem: A): This

  /** Creates a new set with a given element removed from this set.
   *
   *  @param elem the element to be removed
   *  @return a new set that contains all elements of this set but that does not
   *          contain `elem`.
   */
  def - (elem: A): This

  /** Tests if this set is empty.
   *
   *  @return `true` if there is no element in the set, `false` otherwise.
   */
  override def isEmpty: Boolean = size == 0

  /** Tests if some element is contained in this set.
   *
   *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
   *  @param elem the element to test for membership.
   *  @return  `true` if `elem` is contained in this set, `false` otherwise.
   */
  def apply(elem: A): Boolean = contains(elem)

  /** Computes the intersection between this set and another set.
   *
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`.
   */
  def intersect(that: Set[A]): This = filter(that.contains)

  /** Computes the intersection between this set and another set.
   *
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`.
   *  @note  Same as `intersect`.
   */
  def &(that: Set[A]): This = intersect(that)

 /**  This method is an alias for `intersect`.
   *  It computes an intersection with set `that`.
   *  It removes all the elements that are not present in `that`.
   *
   *  @param that the set to intersect with
   */
  @deprecated("use & instead") def ** (that: Set[A]): This = intersect(that)

  /** Computes the union between of set and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: Set[A]): This = this.++(that)

  /** Computes the union between this set and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`.
   *  @note       Same as `union`.
   */
  def | (that: Set[A]): This = union(that)

  /** Computes the difference of this set and another set.
   *
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def diff(that: Set[A]): This = --(that)

  /** The difference of this set and another set.
   *
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   *  @note       Same as `diff`.
   */
  def &~(that: Set[A]): This = diff(that)

  /** Tests whether this set is a subset of another set.
   *
   *  @param that  the set to test.
   *  @return     `true` if this set is a subset of `that`, i.e. if
   *              every element of this set is also an element of `that`.
   */
  def subsetOf(that: Set[A]): Boolean = forall(that.contains)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this set.
   *           Unless overridden this is simply `"Set"`.
   */
  override def stringPrefix: String = "Set"

  override def toString = super[IterableLike].toString
  override def hashCode() = this map (_.hashCode) sum

  /** Compares this set with another object for equality.
   *
   *  @param that the other object
   *  @return     `true` if `that` is a set which contains the same elements
   *              as this set.
   *  @note This operation contains an unchecked cast: if `that`
   *        is a set, it will assume with an unchecked cast
   *        that it has the same element type as this set.
   *        Any subsequent ClassCastException is treated as a `false` result.
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
