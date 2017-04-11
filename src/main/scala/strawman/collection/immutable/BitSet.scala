package strawman
package collection.immutable

import strawman.collection.BoundedIterableFactory
import strawman.collection.mutable.Builder

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]
    with SetMonoTransforms[Int, BitSet] // Override mono transforms ops to return a BitSet rather than a SortedSet[Int]

object BitSet extends BoundedIterableFactory[Int] {
  type To[_] = BitSet

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet = ???

  def newBuilder[E <: Int]: Builder[E, BitSet] = ???

  def empty[A <: Int]: BitSet = ???
}
