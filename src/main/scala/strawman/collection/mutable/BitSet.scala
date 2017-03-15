package strawman
package collection.mutable

import strawman.collection.Build

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]

object BitSet extends Build[Int] {
  type To[_] = BitSet

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[E <: Int]: Builder[E, BitSet] = ???
}
