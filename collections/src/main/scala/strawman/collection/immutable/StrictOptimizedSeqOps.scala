package strawman
package collection
package immutable

import scala.{Any, IndexOutOfBoundsException, Int}

/**
  * Trait that overrides operations to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps[+A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with collection.StrictOptimizedSeqOps[A, CC, C] {

  override def prepend[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b += elem
    b ++= toIterable
    b.result()
  }

  override def append[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b ++= toIterable
    b += elem
    b.result()
  }

  override def updated[B >: A](index: Int, elem: B): CC[B] = {
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size)
    }
    var i = 0
    val it = toIterable.iterator()
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

  override def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    var i = 0
    val it = toIterable.iterator()
    while (i < from && it.hasNext) {
      b += it.next()
      i += 1
    }
    b ++= other
    i = replaced
    while (i > 0 && it.hasNext) {
      it.next()
      i -= 1
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

}
