package strawman
package collection
package mutable

import scala.{Option, `inline`}

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

  def put(key: K, value: V): Option[V]

}

object Map extends MapFactory.Delegate[Map](HashMap)