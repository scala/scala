package strawman.collection.mutable

import scala.{`inline`, Option}

/** Base type of mutable Maps */
trait Map[K, V] extends strawman.collection.Map[K, V] with MapLike[K, V, Map]

/** Base trait of mutable Maps implementations */
trait MapLike[K, V, +CC[X, Y] <: Map[X, Y]]
  extends strawman.collection.MapLike[K, V, CC]
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
