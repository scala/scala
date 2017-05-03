package strawman
package collection
package immutable

import strawman.collection.mutable.Builder
import scala.Ordering

/** Base trait for sorted sets */
trait SortedSet[A]
  extends Set[A]
    with collection.SortedSet[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +CC[X] <: SortedSet[X] with SortedSetLike[X, CC]]
  extends SetLike[A, Set]
     with SetOps[A, CC[A]]
     with collection.SortedSetLike[A, CC]

object SortedSet extends OrderedSetFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = TreeSet.empty
  def orderedNewBuilder[A : Ordering]: Builder[A, SortedSet[A]] = TreeSet.orderedNewBuilder
  def orderedFromIterable[E : Ordering](it: collection.Iterable[E]): SortedSet[E] = TreeSet.orderedFromIterable(it)
}
