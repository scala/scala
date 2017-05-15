package strawman
package collection
package mutable

import scala.{Option, `inline`, Unit}

/** Base type of mutable Maps */
trait Map[K, V] extends Iterable[(K, V)]
                   with collection.Map[K, V]
                   with MapOps[K, V, Map, Map[K, V]]

/** Base trait of mutable Maps implementations */
trait MapOps[K, V, +CC[X, Y] <: Map[X, Y], +C <: Map[K, V]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with Growable[(K, V)] {

  def fromIterable[B](coll: collection.Iterable[B]): Iterable[B] = Iterable.fromIterable(coll)

  /** Removes a single element from this $coll.
    *
    *  @param key  the key of the element to remove.
    *  @return the $coll itself
    */
  def remove(key: K): this.type
  /** Alias for `remove` */
  @`inline` final def -= (key: K): this.type = remove(key)

  /** Adds a new key/value pair to this map and optionally returns previously bound value.
    *  If the map already contains a
    *  mapping for the key, it will be overridden by the new value.
    *
    * @param key    the key to update
    * @param value  the new value
    * @return an option value containing the value associated with the key
    *         before the `put` operation was executed, or `None` if `key`
    *         was not defined in the map before.
    */
  def put(key: K, value: V): Option[V] = {
    val r = get(key)
    update(key, value)
    r
  }

  /** Adds a new key/value pair to this map.
    *  If the map already contains a
    *  mapping for the key, it will be overridden by the new value.
    *
    *  @param key    The key to update
    *  @param value  The new value
    */
  def update(key: K, value: V): Unit = { this += ((key, value)) }

  override def clone(): C = empty ++= coll

}

object Map extends MapFactory.Delegate[Map](HashMap)