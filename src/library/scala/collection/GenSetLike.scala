/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection


/** A template trait for sets which may possibly
 *  have their operations implemented in parallel.
 *
 *  @define Coll GenSet
 *  @define coll general set
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @define setNote
 *
 *  A set is a collection that contains no duplicate elements.
 */
trait GenSetLike[A, +Repr]
extends GenIterableLike[A, Repr]
   with (A => Boolean)
   with Equals
   with Parallelizable[A, parallel.ParSet[A]] {

  def iterator: Iterator[A]
  def contains(elem: A): Boolean
  def +(elem: A): Repr
  def -(elem: A): Repr

  def seq: Set[A]

  /** Tests if some element is contained in this set.
   *
   *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
   *  @param elem the element to test for membership.
   *  @return  `true` if `elem` is contained in this set, `false` otherwise.
   */
  def apply(elem: A): Boolean = this contains elem

  /** Computes the intersection between this set and another set.
   *
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`.
   */
  def intersect(that: GenSet[A]): Repr = this filter that

  /** Computes the intersection between this set and another set.
   *
   *  '''Note:'''  Same as `intersect`.
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`.
   */
  def &(that: GenSet[A]): Repr = this intersect that

  /** Computes the union between of set and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: GenSet[A]): Repr

  /** Computes the union between this set and another set.
   *
   *  '''Note:'''  Same as `union`.
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def | (that: GenSet[A]): Repr = this union that

  /** Computes the difference of this set and another set.
   *
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def diff(that: GenSet[A]): Repr

  /** The difference of this set and another set.
   *
   *  '''Note:'''  Same as `diff`.
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def &~(that: GenSet[A]): Repr = this diff that

  /** Tests whether this set is a subset of another set.
   *
   *  @param that  the set to test.
   *  @return     `true` if this set is a subset of `that`, i.e. if
   *              every element of this set is also an element of `that`.
   */
  def subsetOf(that: GenSet[A]): Boolean = this forall that

  /** Compares this set with another object for equality.
   *
   *  '''Note:''' This operation contains an unchecked cast: if `that`
   *        is a set, it will assume with an unchecked cast
   *        that it has the same element type as this set.
   *        Any subsequent ClassCastException is treated as a `false` result.
   *  @param that the other object
   *  @return     `true` if `that` is a set which contains the same elements
   *              as this set.
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenSet[_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) &&
      (try this subsetOf that.asInstanceOf[GenSet[A]]
       catch { case ex: ClassCastException => false })
    case _ =>
      false
  }

  // Careful! Don't write a Set's hashCode like:
  //    override def hashCode() = this map (_.hashCode) sum
  // Calling map on a set drops duplicates: any hashcode collisions would
  // then be dropped before they can be added.
  // Hash should be symmetric in set entries, but without trivial collisions.
  override def hashCode()= scala.util.hashing.MurmurHash3.setHash(seq)
}
