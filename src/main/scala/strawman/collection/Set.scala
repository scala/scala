package strawman
package collection

import scala.{Any, Array, Boolean, Equals, Int, NoSuchElementException, `inline`, throws}
import scala.Predef.intWrapper
import scala.util.hashing.MurmurHash3
import java.lang.String

/** Base trait for set collections.
  */
trait Set[A] extends Iterable[A] with SetOps[A, Set, Set[A]] {
  final protected[this] def coll: this.type = this
}

/** Base trait for set operations */
trait SetOps[A, +CC[_], +C <: SetOps[A, CC, C]]
  extends IterableOps[A, CC, C]
     with (A => Boolean)
     with Equals {

  def contains(elem: A): Boolean

  /** Tests if some element is contained in this set.
    *
    *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
    *  @param elem the element to test for membership.
    *  @return  `true` if `elem` is contained in this set, `false` otherwise.
    */
  @`inline` final def apply(elem: A): Boolean = this.contains(elem)

  /** Tests whether this set is a subset of another set.
    *
    *  @param that  the set to test.
    *  @return     `true` if this set is a subset of `that`, i.e. if
    *              every element of this set is also an element of `that`.
    */
  def subsetOf(that: Set[A]): Boolean = this.forall(that)

  /** An iterator over all subsets of this set of the given size.
    *  If the requested size is impossible, an empty iterator is returned.
    *
    *  @param len  the size of the subsets.
    *  @return     the iterator.
    */
  def subsets(len: Int): Iterator[C] = {
    if (len < 0 || len > size) Iterator.empty
    else new SubsetsItr(toIterable.to(IndexedSeq), len)
  }

  /** An iterator over all subsets of this set.
    *
    *  @return     the iterator.
    */
  def subsets(): Iterator[C] = new Iterator[C] {
    private val elms = toIterable.to(IndexedSeq)
    private var len = 0
    private var itr: Iterator[C] = Iterator.empty

    def hasNext = len <= elms.size || itr.hasNext
    def next() = {
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
    */
  private class SubsetsItr(elms: IndexedSeq[A], len: Int) extends Iterator[C] {
    private val idxs = Array.range(0, len+1)
    private var _hasNext = true
    idxs(len) = elms.size

    def hasNext = _hasNext
    @throws[NoSuchElementException]
    def next(): C = {
      if (!hasNext) Iterator.empty.next()

      val buf = newSpecificBuilder()
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

  def canEqual(that: Any) = true

  override def equals(that: Any): Boolean =
    that match {
      case set: Set[A] =>
        (this eq set) ||
          (set canEqual this) &&
            (toIterable.size == set.size) &&
            (this subsetOf set)
      case _ => false
    }

  override def hashCode(): Int = Set.setHash(toIterable)

  override def toString(): String = super[IterableOps].toString() // Because `Function1` overrides `toString` too

  /** Computes the intersection between this set and another set.
    *
    *  @param   that  the set to intersect with.
    *  @return  a new set consisting of all elements that are both in this
    *  set and in the given set `that`.
    */
  def intersect(that: Set[A]): C = this.filter(that)

  /** Alias for `intersect` */
  @`inline` final def & (that: Set[A]): C = intersect(that)

  /** Computes the difference of this set and another set.
    *
    *  @param that the set of elements to exclude.
    *  @return     a set containing those elements of this
    *              set that are not also contained in the given set `that`.
    */
  def diff(that: Set[A]): C

  /** Alias for `diff` */
  @`inline` final def &~ (that: Set[A]): C = this diff that

  /** Creates a new $coll by adding all elements contained in another collection to this $coll, omitting duplicates.
    *
    * This method takes a collection of elements and adds all elements, omitting duplicates, into $coll.
    *
    * Example:
    *  {{{
    *    scala> val a = Set(1, 2) concat Set(2, 3)
    *    a: scala.collection.immutable.Set[Int] = Set(1, 2, 3)
    *  }}}
    *
    *  @param that     the collection containing the elements to add.
    *  @return a new $coll with the given elements added, omitting duplicates.
    */
  def concat(that: collection.Iterable[A]): C = fromSpecificIterable(View.Concat(toIterable, that))

  /** Alias for `concat` */
  @`inline` final def ++ (that: collection.Iterable[A]): C = concat(that)

  /** Computes the union between of set and another set.
    *
    *  @param   that  the set to form the union with.
    *  @return  a new set consisting of all elements that are in this
    *  set or in the given set `that`.
    */
  @`inline` final def union(that: collection.Iterable[A]): C = concat(that)

  /** Alias for `union` */
  @`inline` final def | (that: collection.Iterable[A]): C = concat(that)

  /** The empty set of the same type as this set
    * @return  an empty set of type `C`.
    */
  def empty: C
}

object Set extends IterableFactory.Delegate[Set](immutable.Set) {

  // Temporary, TODO move to MurmurHash3
  def setHash(xs: Iterable[_]): Int = unorderedHash(xs, "Set".##)

  final def unorderedHash(xs: Iterable[_], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    xs foreach { x =>
      val h = x.##
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h = seed
    h = MurmurHash3.mix(h, a)
    h = MurmurHash3.mix(h, b)
    h = MurmurHash3.mixLast(h, c)
    MurmurHash3.finalizeHash(h, n)
  }

}
