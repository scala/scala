package strawman
package collection.mutable

import strawman.collection.SpecificIterableFactory

import scala.{Array, Int, Long}
import scala.Predef.???

trait BitSet
  extends SortedSet[Int]
     with collection.BitSet
     with collection.BitSetLike[BitSet] {

  private[collection] def elems: Array[Long]

}

object BitSet extends SpecificIterableFactory[Int, BitSet] {
  def fromSpecificIterable(it: strawman.collection.Iterable[Int]): BitSet = ???
  def empty: BitSet = ???
}
