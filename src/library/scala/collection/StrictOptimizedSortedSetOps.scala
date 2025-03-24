/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

/**
  * Trait that overrides sorted set operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedSortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SortedSetOps[A, CC, C]
    with StrictOptimizedSetOps[A, Set, C] {

  override def map[B](f: A => B)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedMap(sortedIterableFactory.newBuilder, f)

  override def flatMap[B](f: A => IterableOnce[B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedFlatMap(sortedIterableFactory.newBuilder, f)

  override def zip[B](that: IterableOnce[B])(implicit @implicitNotFound(SortedSetOps.zipOrdMsg) ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] =
    strictOptimizedZip(that, sortedIterableFactory.newBuilder[(A, B)])

  override def collect[B](pf: PartialFunction[A, B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedCollect(sortedIterableFactory.newBuilder, pf)

}
