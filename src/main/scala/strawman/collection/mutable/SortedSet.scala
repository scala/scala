package strawman
package collection.mutable

import scala.Ordering

import strawman.collection.ConstrainedIterablePolyTransforms

trait SortedSet[A]
  extends collection.SortedSet[A]
    with Set[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends collection.SortedSetLike[A, C]
    with ConstrainedIterablePolyTransforms[A, Set, SortedSet]
    with SetLike[A, Set]
    with SetMonoTransforms[A, C[A]]
