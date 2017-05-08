package strawman
package collection
package immutable

import strawman.collection.MapFactory
import strawman.collection.mutable.Builder

import scala.`inline`

/** Base type of immutable Maps */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapOps[K, V, Map, Map[K, V]]

/** Base trait of immutable Maps implementations */
trait MapOps[K, +V, +CC[X, +Y] <: Map[X, Y] with MapOps[X, Y, CC, _], +C <: Map[K, V]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C] {

  protected def coll: CC[K, V]

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
  def empty[K, V]: Map[K, V] = ListMap.empty[K, V]
}
