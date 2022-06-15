/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

/**
  * Trait that overrides operations to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps[+A, +CC[_], +C]
  extends Any
    with SeqOps[A, CC, C]
    with collection.StrictOptimizedSeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def distinctBy[B](f: A => B): C = {
    if (lengthCompare(1) <= 0) coll
    else {
      val builder = newSpecificBuilder
      val seen = mutable.HashSet.empty[B]
      val it = this.iterator
      var different = false
      while (it.hasNext) {
        val next = it.next()
        if (seen.add(f(next))) builder += next else different = true
      }
      if (different) builder.result() else coll
    }
  }

  override def updated[B >: A](index: Int, elem: B): CC[B] = {
    if (index < 0) throw new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${if (knownSize>=0) knownSize else "unknown"})")
    val b = iterableFactory.newBuilder[B]
    if (knownSize >= 0) {
      b.sizeHint(size)
    }
    var i = 0
    val it = iterator
    while (i < index && it.hasNext) {
      b += it.next()
      i += 1
    }
    if (!it.hasNext) throw new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${i-1})")
    b += elem
    it.next(): Unit
    while (it.hasNext) b += it.next()
    b.result()
  }

  override def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    var i = 0
    val it = iterator
    while (i < from && it.hasNext) {
      b += it.next()
      i += 1
    }
    b ++= other
    i = replaced
    while (i > 0 && it.hasNext) {
      it.next(): Unit
      i -= 1
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  override def sorted[B >: A](implicit ord: Ordering[B]): C = super.sorted(ord)

}
