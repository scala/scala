package scala.collection

import scala.language.higherKinds

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

}
