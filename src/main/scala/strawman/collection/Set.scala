package strawman
package collection

import scala.{Any, Boolean, Equals, `inline`, Int}
import scala.util.hashing.MurmurHash3

/** Base trait for set collections.
  */
trait Set[A] extends Iterable[A] with SetOps[A, Set, Set[A]]

/** Base trait for set operations */
trait SetOps[A, +CC[X], +C <: Set[A]]
  extends IterableOps[A, CC, C]
     with (A => Boolean)
     with Equals {

  protected def coll: C

  def contains(elem: A): Boolean

  /** Tests if some element is contained in this set.
    *
    *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
    *  @param elem the element to test for membership.
    *  @return  `true` if `elem` is contained in this set, `false` otherwise.
    */
  @`inline` final def apply(elem: A): Boolean = this.contains(elem)

  def subsetOf(that: Set[A]): Boolean = this.forall(that)

  def canEqual(that: Any) = true

  override def equals(that: Any): Boolean =
    that match {
      case set: Set[A] =>
        (this eq set) ||
          (set canEqual this) &&
            (coll.size == set.size) &&
            (this subsetOf set)
      case _ => false
    }

  override def hashCode(): Int = Set.setHash(coll)

  /** Computes the intersection between this set and another set.
    *
    *  @param   that  the set to intersect with.
    *  @return  a new set consisting of all elements that are both in this
    *  set and in the given set `that`.
    */
  def intersect(that: Set[A]): C = this.filter(that)

  /** Alias for `intersect` */
  @`inline` final def & (that: Set[A]): C = intersect(that)

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
  def concat(that: collection.IterableOnce[A]): C = fromSpecificIterable(View.Concat(coll, that))

  /** Alias for `concat` */
  @`inline` final def ++ (that: collection.IterableOnce[A]): C = concat(that)

  /** Computes the union between of set and another set.
    *
    *  @param   that  the set to form the union with.
    *  @return  a new set consisting of all elements that are in this
    *  set or in the given set `that`.
    */
  @`inline` final def union(that: collection.IterableOnce[A]): C = concat(that)

  /** Alias for `union` */
  @`inline` final def | (that: collection.IterableOnce[A]): C = concat(that)

  /** The empty set of the same type as this set
    * @return  an empty set of type `C`.
    */
  def empty: C
}

object Set extends IterableFactory[Set] {
  def empty[A <: Any]: Set[A] = immutable.Set.empty

  def fromIterable[E](it: Iterable[E]): Set[E] =
    it match {
      case s: Set[E] => s
      case _         => empty ++ it
    }

  // Temporary, TODO move to MurmurHash3
  def setHash(xs: Set[_]): Int = unorderedHash(xs, "Set".##)

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
