package strawman.collection

import scala.Ordering

/** Base type of sorted sets */
trait SortedSet[A]
  extends Set[A]
    with Sorted[A]
    with SortedSetLike[A, SortedSet] // Inherited SortedSet operations return a `SortedSet`

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C[A]]
    with ConstrainedIterablePolyTransforms[A, Set, C]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetMonoTransforms[A, C[A]] { // Override the return type of Set ops to return C[A]

  type Ev[X] = Ordering[X]

  def firstKey: A = head

  def lastKey: A = last

}
