package strawman
package collection.mutable

import strawman.collection.TypeConstrainedFromIterable

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]

object BitSet extends TypeConstrainedFromIterable[Int] {
  type To[_] = BitSet

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[E <: Int]: Builder[E, BitSet] = ???
}
