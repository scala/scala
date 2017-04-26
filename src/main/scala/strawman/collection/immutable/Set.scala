package strawman
package collection
package immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.{Any, `inline`}

/** Base trait for immutable set collections */
trait Set[A] extends Iterable[A] with collection.Set[A] with SetOps[A, Set, Set[A]]

/** Base trait for immutable set operations */
trait SetOps[A, +CC[X], +C <: Set[A] with SetOps[A, Set, C]]
  extends collection.IterableOps[A, CC, C] {

  protected def coll: C

  /** Creates a new set with an additional element, unless the element is
    *  already present.
    *
    *  @param elem the element to be added
    *  @return a new set that contains all elements of this set and that also
    *          contains `elem`.
    */
  def incl(elem: A): C

  /** Alias for `incl` */
  @`inline` final def + (elem: A): C = incl(elem)

  /** Creates a new set with a given element removed from this set.
    *
    *  @param elem the element to be removed
    *  @return a new set that contains all elements of this set but that does not
    *          contain `elem`.
    */
  def excl(elem: A): C

  /** Alias for `excl` */
  @`inline` final def - (elem: A): C = excl(elem)

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
  def concat(that: collection.IterableOnce[A]): C = {
    var result: C = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

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

}

object Set extends IterableFactory[Set] {
  def empty[A]: Set[A] = ListSet.empty
  def fromIterable[E](it: strawman.collection.Iterable[E]): Set[E] = ListSet.fromIterable(it)
}
