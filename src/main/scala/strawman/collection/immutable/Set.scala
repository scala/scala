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

  /** Alias for `add` */
  @`inline` final def + (elem: A): C = incl(elem)

  /** Creates a new set with a given element removed from this set.
    *
    *  @param elem the element to be removed
    *  @return a new set that contains all elements of this set but that does not
    *          contain `elem`.
    */
  def excl(elem: A): C

  /** Alias for `remove` */
  @`inline` final def - (elem: A): C = excl(elem)

  def union(that: collection.Set[A]): C = {
    var result: C = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }
}

object Set extends IterableFactory[Set] {
  def empty[A <: Any]: Set[A] = ListSet.empty
  def fromIterable[E](it: strawman.collection.Iterable[E]): Set[E] = ListSet.fromIterable(it)
}
