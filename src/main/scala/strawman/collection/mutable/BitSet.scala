package strawman
package collection.mutable

import strawman.collection.BoundedIterableFactory

import scala.{Array, Int, Long}
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int] {

  private[collection] def elems: Array[Long]

}

object BitSet extends BoundedIterableFactory[Int] {
  type To[_] = BitSet

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[E <: Int]: Builder[E, BitSet] = ???

  def empty[A <: Int]: BitSet = ???

}
