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

package scala.collection

import scala.annotation.implicitNotFound

/**
  * Trait that overrides sorted map operations to take advantage of strict builders.
  *
  * @tparam K  Type of keys
  * @tparam V  Type of values
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedSortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends SortedMapOps[K, V, CC, C]
    with StrictOptimizedMapOps[K, V, Map, C] {

  override def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedMap(sortedMapFactory.newBuilder, f)

  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedFlatMap(sortedMapFactory.newBuilder, f)

  override def concat[V2 >: V](xs: IterableOnce[(K, V2)]): CC[K, V2] =
    strictOptimizedConcat(xs, sortedMapFactory.newBuilder(using ordering))

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedCollect(sortedMapFactory.newBuilder, pf)

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] = {
    val m = ((this + elem1).asInstanceOf[Map[K, V]] + elem2).asInstanceOf[CC[K, V1]]
    if(elems.isEmpty) m else m.concat(elems).asInstanceOf[CC[K, V1]]
  }
}
