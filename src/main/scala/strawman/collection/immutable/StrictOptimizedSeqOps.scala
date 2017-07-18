package strawman
package collection
package immutable

import scala.{Any, IndexOutOfBoundsException, Int}

/**
  * Trait that overrides operations to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps[+A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def prepend[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b += elem
    b ++= coll
    b.result()
  }

  override def append[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]()
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b ++= coll
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

}
