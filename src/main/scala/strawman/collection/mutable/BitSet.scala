package strawman
package collection.mutable

import strawman.collection.MonomorphicIterableFactory

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]

object BitSet extends MonomorphicIterableFactory[Int, BitSet] {

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[A <: Int]: Builder[A, BitSet] = ???
}
