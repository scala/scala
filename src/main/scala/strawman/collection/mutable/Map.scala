package strawman.collection.mutable

import strawman.collection.IterableMonoTransforms

import scala.Option

/** Base type of mutable Maps */
trait Map[K, V]
  extends strawman.collection.Map[K, V]
    with MapLike[K, V, Map]

/** Base trait of mutable Maps implementations */
trait MapLike[K, V, +C[X, Y] <: Map[X, Y]]
  extends strawman.collection.MapLike[K, V, C]
    with Iterable[(K, V)]
    with Growable[(K, V)] {

  def -= (elem: (K, V)): this.type

  def put(key: K, value: V): Option[V]

}
