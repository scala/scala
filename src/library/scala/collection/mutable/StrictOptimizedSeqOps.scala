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
package mutable

import scala.language.higherKinds

trait StrictOptimizedSeqOps [A, +CC[_], +C <: AnyRef]
  extends SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

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