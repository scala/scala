package strawman
package collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.OrderedIterableFactory
import scala.Ordering

/** Base trait for sorted sets */
trait SortedSet[A]
  extends collection.SortedSet[A]
    with Set[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends collection.SortedSetLike[A, C]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetMonoTransforms[A, C[A]] // Override the return type of Set ops to return C[A]

object SortedSet extends OrderedIterableFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = TreeSet.empty
  def orderedNewBuilder[A : Ordering]: Builder[A, SortedSet[A]] = TreeSet.orderedNewBuilder
  def orderedFromIterable[E : Ordering](it: collection.Iterable[E]): SortedSet[E] = TreeSet.orderedFromIterable(it)
}
