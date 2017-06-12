package strawman
package collection.generic

import strawman.collection.mutable.Builder

import scala.{Any, IndexOutOfBoundsException, Int}

object Updated {

  /** Implementation of the `updated` method for strict collections */
  def strict[A, C](index: Int, elem: A, coll: collection.immutable.Seq[A], newBuilder: () => Builder[A, C]): C = {
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    val b = newBuilder()
    var i = 0
    val it = coll.iterator()
    while (i < index && it.hasNext) {
      b += it.next()
      i += 1
    }
    if (!it.hasNext) throw new IndexOutOfBoundsException(index.toString)
    b += elem
    it.next()
    while (it.hasNext) b += it.next()
    b.result()
  }

  def toImmutableArray[A](index: Int, elem: A, coll: collection.immutable.IndexedSeq[A]): collection.immutable.ImmutableArray[A] = {
    val arr = scala.Array.ofDim[Any](coll.size)
    val it = coll.iterator()
    var i = 0
    while (it.hasNext) {
      arr(i) = it.next()
      i += 1
    }
    arr(index) = elem
    new collection.immutable.ImmutableArray[A](arr)
  }

}
