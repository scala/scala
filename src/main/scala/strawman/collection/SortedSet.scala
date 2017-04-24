package strawman.collection

import strawman.collection.mutable.Builder

import scala.Ordering

/** Base type of sorted sets */
trait SortedSet[A]
  extends Set[A]
    with Sorted[A]
    with SortedSetLike[A, SortedSet] // Inherited SortedSet operations return a `SortedSet`

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C[A]]
    with OrderedIterablePolyTransforms[A, Set, C]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetMonoTransforms[A, C[A]] { // Override the return type of Set ops to return C[A]

  type Ev[X] = Ordering[X]

  def firstKey: A = head

  def lastKey: A = last

}

object SortedSet extends OrderedIterableFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = immutable.SortedSet.empty
  def orderedNewBuilder[A : Ordering]: Builder[A, SortedSet[A]] = immutable.SortedSet.orderedNewBuilder
  def orderedFromIterable[E : Ordering](it: Iterable[E]): SortedSet[E] = immutable.SortedSet.orderedFromIterable(it)
}
