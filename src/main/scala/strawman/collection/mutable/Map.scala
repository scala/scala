package strawman.collection.mutable

import strawman.collection.IterableMonoTransforms

import scala.{inline, Option}

/** Base type of mutable Maps */
trait Map[K, V]
  extends strawman.collection.Map[K, V]
    with MapLike[K, V, Map]

/** Base trait of mutable Maps implementations */
trait MapLike[K, V, +C[X, Y] <: Map[X, Y]]
  extends strawman.collection.MapLike[K, V, C]
    with Iterable[(K, V)]
    with Growable[(K, V)] {

  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def removeInPlace(elem: (K, V)): this.type
  /** Alias for `removeInPlace` */
  @inline final def -= (elem: (K, V)): this.type = removeInPlace(elem)

  def put(key: K, value: V): Option[V]

}
