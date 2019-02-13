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

  override def prepended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b += elem
    b ++= this
    b.result()
  }

  override def appended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    if (knownSize >= 0) {
      b.sizeHint(size + 1)
    }
    b ++= this
    b += elem
    b.result()
  }

  override def appendedAll[B >: A](suffix: IterableOnce[B]): CC[B] =
    strictOptimizedConcat(suffix, iterableFactory.newBuilder)

  override def prependedAll[B >: A](prefix: IterableOnce[B]): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b ++= prefix
    b ++= this
    b.result()
  }

  // Must override here to ensure that `concat` is still an alias for `appendedAll` since it is overridden in `StrictOptimizedIterableOps`
  @deprecatedOverriding("Compatibility override", since="2.13.0")
  override def concat[B >: A](suffix: IterableOnce[B]): CC[B] = appendedAll(suffix)

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

  override def diff[B >: A](that: Seq[B]): C = {
    val occ = occCounts(that)
    val b = newSpecificBuilder
    for (x <- this) {
      val ox = occ(x)  // Avoid multiple map lookups
      if (ox == 0) b += x
      else occ(x) = ox - 1
    }
    b.result()
  }

  override def intersect[B >: A](that: Seq[B]): C = {
    val occ = occCounts(that)
    val b = newSpecificBuilder
    for (x <- this) {
      val ox = occ(x)  // Avoid multiple map lookups
      if (ox > 0) {
        b += x
        occ(x) = ox - 1
      }
    }
    b.result()
  }
}
