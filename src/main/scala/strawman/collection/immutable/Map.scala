package strawman
package collection.immutable

import strawman.collection.IterableMonoTransforms

/** Base type of immutable Maps */
trait Map[K, +V]
  extends collection.Map[K, V]
    with MapLike[K, V, Map]

/** Base trait of immutable Maps implementations */
trait MapLike[K, +V, +C[X, +Y] <: Map[X, Y]]
  extends collection.MapLike[K, V, C]
    with MapMonoTransforms[K, V, C[K, V]]
    with Iterable[(K, V)]

/** Immutable Map operations returning a self-like Map */
trait MapMonoTransforms[K, +V, +Repr <: Map[K, V]]
  extends IterableMonoTransforms[(K, V), Repr] {

  /**
    * Removes a key from this map, returning a new map.
    *
    * @param key the key to be removed
    * @return a new map without a binding for ''key''
    */
  def - (key: K): Repr

}