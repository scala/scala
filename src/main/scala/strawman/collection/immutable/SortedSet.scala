package strawman.collection.immutable

import strawman.collection.{SetMonoTransforms, Sorted, SortedLike}

/** Base trait for sorted sets */
trait SortedSet[A]
  extends Set[A]
    with Sorted[A]
    with SortedSetLike[A, SortedSet] // Inherited SortedSet operations return a `SortedSet`

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetOps[A, C[A]] // Override the return type of Set ops to return C[A]
    with SetMonoTransforms[A, C[A]] {

  // We have to write again the definition of methods inherited from SortedPolyTransforms
  // so that they take precedence over the one inherited from IterablePolyTransforms
  def map[B](f: A => B)(implicit ordering: scala.Ordering[B]): C[B]

}
