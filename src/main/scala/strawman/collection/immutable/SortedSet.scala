package strawman
package collection
package immutable

import strawman.collection.mutable.Builder
import scala.Ordering

/** Base trait for sorted sets */
trait SortedSet[A]
  extends Set[A]
     with collection.SortedSet[A]
     with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A,
                   +CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _],
                   +C <: SortedSet[A] with SortedSetOps[A, SortedSet, C]]
  extends SetOps[A, Set, C]
     with collection.SortedSetOps[A, CC, C]

object SortedSet extends OrderedSetFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = TreeSet.empty
  def orderedFromIterable[E : Ordering](it: collection.Iterable[E]): SortedSet[E] = TreeSet.orderedFromIterable(it)
}
