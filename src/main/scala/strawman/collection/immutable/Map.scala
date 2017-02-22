package strawman
package collection.immutable

/** Base type of immutable Maps */
trait Map[K, +V]
  extends collection.Map[K, V]
    with MapLike[K, V, Map]

/** Base trait of immutable Maps implementations */
trait MapLike[K, +V, +C[X, +Y] <: Map[X, Y]]
  extends collection.MapLike[K, V, C]
    with Iterable[(K, V)] {

  /**
    * Add a key/value pair to this map, returning a new map.
    *
    * @param kv the key/value pair.
    * @tparam V1 the type of the value in the key/value pair.
    * @return A new map with the new binding added to this map.
    */
  def + [V1 >: V](kv: (K, V1)): C[K, V1]

  /**
    * Removes a key from this map, returning a new map.
    *
    * @param key the key to be removed
    * @return a new map without a binding for ''key''
    */
  def - (key: K): C[K, V]

}