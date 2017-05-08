package strawman
package collection
package mutable

import scala.{`inline`, Option}

/** Base type of mutable Maps */
trait Map[K, V] extends Iterable[(K, V)]
                   with collection.Map[K, V]
                   with MapOps[K, V, Map, Map[K, V]]

/** Base trait of mutable Maps implementations */
trait MapOps[K, V, +CC[X, Y] <: Map[X, Y], +C <: Map[K, V]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with Growable[(K, V)] {

  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def remove(elem: (K, V)): this.type
  /** Alias for `remove` */
  @`inline` final def -= (elem: (K, V)): this.type = remove(elem)

  def put(key: K, value: V): Option[V]

}
