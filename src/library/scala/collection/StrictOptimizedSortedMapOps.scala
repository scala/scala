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
    strictOptimizedConcat(xs, sortedMapFactory.newBuilder)

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedCollect(sortedMapFactory.newBuilder, pf)

}
