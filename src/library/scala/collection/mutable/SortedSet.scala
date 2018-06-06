package scala
package collection
package mutable

import scala.language.higherKinds

/**
  * Base type for mutable sorted set collections
  */
trait SortedSet[A]
  extends Set[A]
    with collection.SortedSet[A]
    with SortedSetOps[A, SortedSet, SortedSet[A]] {

  override def sortedIterableFactory: SortedIterableFactory[SortedIterableCC] = SortedSet
}

/**
  * @define coll mutable sorted set
  * @define Coll `mutable.Sortedset`
  */
trait SortedSetOps[A, +CC[X] <: SortedSetOps[X, CC, _] with SortedSet[X], +C <: SortedSetOps[A, CC, C] with CC[A]]
  extends SetOps[A, Set, C]
    with collection.SortedSetOps[A, CC, C]

/**
  * $factoryInfo
  * @define coll mutable sorted set
  * @define Coll `mutable.Sortedset`
  */
@SerialVersionUID(3L)
object SortedSet extends SortedIterableFactory.Delegate[SortedSet](TreeSet)
