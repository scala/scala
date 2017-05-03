package strawman
package collection.immutable

import strawman.collection.MapFactory
import strawman.collection.mutable.Builder

import scala.`inline`

/** Base type of immutable Maps */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapLike[K, V, Map]

/** Base trait of immutable Maps implementations */
trait MapLike[K, +V, +CC[X, +Y] <: Map[X, Y] with MapLike[X, Y, CC]]
  extends collection.MapLike[K, V, CC]
     with MapOps[K, V, CC[K, V]]
     with MapMappings[K, V, CC]

/** Immutable Map operations returning a self-like Map */
trait MapOps[K, +V, +C] extends collection.MapOps[K, V, C] {

  /** Removes a key from this map, returning a new map.
    *
    * @param key the key to be removed
    * @return a new map without a binding for ''key''
    */
  def remove(key: K): C

  /** Alias for `remove` */
  @`inline` final def - (key: K): C = remove(key)

  /** The empty map of the same type as this map
    * @return   an empty map of type `Repr`.
    */
  def empty: C
}

trait MapMappings[K, +V, +CC[X, +Y] <: Map[X, Y] with MapLike[X, Y, CC]] extends collection.MapMappings[K, V, CC] {

  protected def coll: CC[K, V]

  /**
    * Add a key/value pair to this map, returning a new map.
    *
    * @param kv the key/value pair.
    * @tparam V1 the type of the value in the key/value pair.
    * @return A new map with the new binding added to this map.
    */
  def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)

  /** Creates a new map obtained by updating this map with a given key/value pair.
    *  @param    key the key
    *  @param    value the value
    *  @tparam   V1 the type of the added value
    *  @return   A new map with the new key/value mapping added to this map.
    *
    *  @usecase  def updated(key: K, value: V): Map[K, V]
    *    @inheritdoc
    */
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]

  override def concat [V1 >: V](that: collection.Iterable[(K, V1)]): CC[K, V1] = {
    var result: CC[K, V1] = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

}

// TODO Special case small maps
object Map extends MapFactory[Map] {
  def newBuilder[K, V]: Builder[(K, V), Map[K, V]] = ListMap.newBuilder[K, V]
  def empty[K, V]: Map[K, V] = ListMap.empty[K, V]
}
