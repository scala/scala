package strawman
package collection
package mutable

import scala.Ordering

/**
  * Base type for mutable sorted set collections
  */
trait SortedSet[A]
  extends Set[A]
    with collection.SortedSet[A]
    with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SetOps[A, Set, C]
    with collection.SortedSetOps[A, CC, C]

object SortedSet
  extends SortedIterableFactory.Delegate[SortedSet](TreeSet)
