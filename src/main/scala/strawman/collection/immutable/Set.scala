package strawman
package collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.{Any, `inline`}

/** Base trait for immutable set collections */
trait Set[A] extends Iterable[A] with collection.Set[A] with SetLike[A, Set]

/** Base trait for immutable set operations */
trait SetLike[A, +CC[X] <: Set[X] with SetLike[X, CC]]
  extends collection.IterableMappings[A, CC] with SetOps[A, CC[A]]

/** Transformation operations returning a Set containing the same kind of
  * elements
  */
trait SetOps[A, +C <: Set[A] with SetOps[A, C]] extends collection.SetOps[A, C] {

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

  override def union(that: collection.Set[A]): C = {
    var result: C = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }
}

object Set extends IterableFactory[Set] {
  def empty[A <: Any]: Set[A] = ListSet.empty
  def newBuilder[A <: Any]: Builder[A, Set[A]] = ListSet.newBuilder
  def fromIterable[E](it: strawman.collection.Iterable[E]): Set[E] = ListSet.fromIterable(it)
}
