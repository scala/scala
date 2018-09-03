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

import scala.annotation.implicitNotFound

/**
  * Trait that overrides sorted map operations to take advantage of strict builders.
  *
  * @tparam K  Type of keys
  * @tparam V  Type of values
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedSortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends SortedMapOps[K, V, CC, C]
    with StrictOptimizedMapOps[K, V, Map, C] {

  override def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedMap(sortedMapFactory.newBuilder[K2, V2], f)

  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedFlatMap(sortedMapFactory.newBuilder[K2, V2], f)

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedCollect(sortedMapFactory.newBuilder[K2, V2], pf)

}
