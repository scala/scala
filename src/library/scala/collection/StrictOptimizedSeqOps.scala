package scala.collection

import scala.language.higherKinds

/**
  * Trait that overrides operations on sequences in order
  * to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps [+A, +CC[_], +C]
  extends Any
    with SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def distinctBy[B](f: A => B): C = {
    val builder = newSpecificBuilder
    val seen = mutable.HashSet.empty[B]
    val it = this.iterator
    while (it.hasNext) {
      val next = it.next()
      if (seen.add(f(next))) builder += next
    }
    builder.result()
  }

  override def +: [B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b += elem
    b ++= this
    b.result()
  }

  override def :+ [B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b ++= this
    b += elem
    b.result()
  }

  override def :++ [B >: A](suffix: Iterable[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b ++= this
    b ++= suffix
    b.result()
  }

  override def ++: [B >: A](prefix: Iterable[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b ++= prefix
    b ++= this
    b.result()
  }

  override def padTo[B >: A](len: Int, elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    val L = size
    b.sizeHint(math.max(L, len))
    var diff = len - L
    b ++= this
    while (diff > 0) {
      b += elem
      diff -= 1
    }
    b.result()
  }

}
