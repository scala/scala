package scala
package collection
package immutable

import scala.language.higherKinds

/** Base trait for sorted sets */
trait SortedSet[A]
  extends Set[A]
     with collection.SortedSet[A]
     with SortedSetOps[A, SortedSet, SortedSet[A]] {

  override def unsorted: Set[A] = this

  override def sortedIterableFactory: SortedIterableFactory[SortedIterableCC] = SortedSet
}

/**
  * @define coll immutable sorted set
  * @define Coll `immutable.SortedSet`
  */
trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SetOps[A, Set, C]
     with collection.SortedSetOps[A, CC, C] {

  def unsorted: Set[A]
}

/**
  * $factoryInfo
  * @define coll immutable sorted set
  * @define Coll `immutable.SortedSet`
  */
@SerialVersionUID(3L)
object SortedSet extends SortedIterableFactory.Delegate[SortedSet](TreeSet)
