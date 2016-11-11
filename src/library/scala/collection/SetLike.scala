/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import mutable.{ Builder, SetBuilder }
import scala.annotation.migration
import parallel.ParSet

/** A template trait for sets.
 *
 *  $setNote
 *    '''Implementation note:'''
 *    This trait provides most of the operations of a `Set` independently of its representation.
 *    It is typically inherited by concrete implementations of sets.
 *  $setTags
 *  @since 2.8
 *
 *  @define setNote
 *
 *  A set is a collection that contains no duplicate elements.
 *
 *    To implement a concrete set, you need to provide implementations of the
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
 *
 * @define setTags
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *
 *  @define coll set
 *  @define Coll Set
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait SetLike[A, +This <: SetLike[A, This] with Set[A]]
extends IterableLike[A, This]
   with GenSetLike[A, This]
   with Subtractable[A, This]
   with Parallelizable[A, ParSet[A]]
{
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
  override protected[this] def newBuilder: Builder[A, This] = new SetBuilder[A, This](empty)

  protected[this] override def parCombiner = ParSet.newCombiner[A]

  // Default collection type appropriate for immutable collections; mutable collections override this
  override def toSeq: Seq[A] = {
    if (isEmpty) Vector.empty[A]
    else {
      val vb = Vector.newBuilder[A]
      foreach(vb += _)
      vb.result
    }
  }

  override def toBuffer[A1 >: A]: mutable.Buffer[A1] = {
    val result = new mutable.ArrayBuffer[A1](size)
    // Faster to let the map iterate itself than to defer through copyToBuffer
    foreach(result += _)
    result
  }

  // note: this is only overridden here to add the migration annotation,
  // which I hope to turn into an Xlint style warning as the migration aspect
  // is not central to its importance.
  @migration("Set.map now returns a Set, so it will discard duplicate values.", "2.8.0")
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That = super.map(f)(bf)

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

  /** Creates a new $coll with additional elements, omitting duplicates.
   *
   *  This method takes two or more elements to be added. Elements that already exist in the $coll will
   *  not be added. Another overloaded variant of this method handles the case where a single element is added.
   *
   *  Example:
   *   {{{
   *    scala> val a = Set(1, 3) + 2 + 3
   *    a: scala.collection.immutable.Set[Int] = Set(1, 3, 2)
   *   }}}
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return   a new $coll with the given elements added, omitting duplicates.
   */
  def + (elem1: A, elem2: A, elems: A*): This = this + elem1 + elem2 ++ elems

  /** Creates a new $coll by adding all elements contained in another collection to this $coll, omitting duplicates.
   *
   * This method takes a collection of elements and adds all elements, omitting duplicates, into $coll.
   *
   * Example:
   *  {{{
   *    scala> val a = Set(1, 2) ++ Set(2, "a")
   *    a: scala.collection.immutable.Set[Any] = Set(1, 2, a)
   *  }}}
   *
   *  @param elems     the collection containing the elements to add.
   *  @return a new $coll with the given elements added, omitting duplicates.
   */
  def ++ (elems: GenTraversableOnce[A]): This = (repr /: elems.seq)(_ + _)

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

  /** Computes the union between of set and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`.
   */
  def union(that: GenSet[A]): This = this ++ that

  /** Computes the difference of this set and another set.
   *
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def diff(that: GenSet[A]): This = this -- that

  /** An iterator over all subsets of this set of the given size.
   *  If the requested size is impossible, an empty iterator is returned.
   *
   *  @param len  the size of the subsets.
   *  @return     the iterator.
   */
  def subsets(len: Int): Iterator[This] = {
    if (len < 0 || len > size) Iterator.empty
    else new SubsetsItr(self.toIndexedSeq, len)
  }

  /** An iterator over all subsets of this set.
   *
   *  @return     the iterator.
   */
  def subsets(): Iterator[This] = new AbstractIterator[This] {
    private val elms = self.toIndexedSeq
    private var len = 0
    private var itr: Iterator[This] = Iterator.empty

    def hasNext = len <= elms.size || itr.hasNext
    def next = {
      if (!itr.hasNext) {
        if (len > elms.size) Iterator.empty.next()
        else {
          itr = new SubsetsItr(elms, len)
          len += 1
        }
      }

      itr.next()
    }
  }

  /** An Iterator including all subsets containing exactly len elements.
   *  If the elements in 'This' type is ordered, then the subsets will also be in the same order.
   *  ListSet(1,2,3).subsets => {{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}
   *
   *  @author Eastsun
   *  @date 2010.12.6
   */
  private class SubsetsItr(elms: IndexedSeq[A], len: Int) extends AbstractIterator[This] {
    private val idxs = Array.range(0, len+1)
    private var _hasNext = true
    idxs(len) = elms.size

    def hasNext = _hasNext
    def next(): This = {
      if (!hasNext) Iterator.empty.next()

      val buf = self.newBuilder
      idxs.slice(0, len) foreach (idx => buf += elms(idx))
      val result = buf.result()

      var i = len - 1
      while (i >= 0 && idxs(i) == idxs(i+1)-1) i -= 1

      if (i < 0) _hasNext = false
      else {
        idxs(i) += 1
        for (j <- (i+1) until len)
          idxs(j) = idxs(j-1) + 1
      }

      result
    }
  }

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this set.
   *           Unless overridden this is simply `"Set"`.
   */
  override def stringPrefix: String = "Set"
  override def toString = super[IterableLike].toString

}
