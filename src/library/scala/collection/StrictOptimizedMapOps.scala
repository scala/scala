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

/**
  * Trait that overrides map operations to take advantage of strict builders.
  *
  * @tparam K  Type of keys
  * @tparam V  Type of values
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedMapOps[K, +V, +CC[_, _] <: IterableOps[_, AnyConstr, _], +C]
  extends MapOps[K, V, CC, C]
    with StrictOptimizedIterableOps[(K, V), Iterable, C] {

  override def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
    strictOptimizedMap(mapFactory.newBuilder, f)

  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] =
    strictOptimizedFlatMap(mapFactory.newBuilder, f)

  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): CC[K, V2] =
    strictOptimizedConcat(suffix, mapFactory.newBuilder)

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): CC[K2, V2] =
    strictOptimizedCollect(mapFactory.newBuilder, pf)

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] = {
    val b = mapFactory.newBuilder[K, V1]
    b ++= this
    b += elem1
    b += elem2
    if (elems.nonEmpty) b ++= elems
    b.result()
  }
}
