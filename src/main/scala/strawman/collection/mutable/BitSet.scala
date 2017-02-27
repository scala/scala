package strawman
package collection.mutable

import strawman.collection.BitSetFactories

import scala.Int
import scala.Predef.???

trait BitSet
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]

object BitSet extends BitSetFactories[BitSet] {

  def newBuilder: Builder[Int, BitSet] = ???

}