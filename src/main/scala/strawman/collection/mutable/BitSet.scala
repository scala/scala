package strawman
package collection
package mutable

import scala.{Array, Int, Long}
import scala.Predef.???

trait BitSet
  extends SortedSet[Int]
     with collection.BitSet
     with SortedSetOps[Int, SortedSet, BitSet]
     with collection.BitSetOps[BitSet] {

  private[collection] def elems: Array[Long]

}

object BitSet extends SpecificIterableFactory[Int, BitSet] {
  def fromSpecificIterable(it: strawman.collection.Iterable[Int]): BitSet = ???
  def empty: BitSet = ???
}
