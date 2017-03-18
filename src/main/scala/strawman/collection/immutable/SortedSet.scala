package strawman
package collection.immutable

import scala.Ordering

import strawman.collection.ConstrainedIterablePolyTransforms

/** Base trait for sorted sets */
trait SortedSet[A]
  extends collection.SortedSet[A]
    with Set[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends collection.SortedSetLike[A, C]
    with ConstrainedIterablePolyTransforms[A, Set, C]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetMonoTransforms[A, C[A]] // Override the return type of Set ops to return C[A]
