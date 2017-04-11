package strawman
package collection.immutable

import strawman.collection.{IterablePolyTransforms, MapPolyTransforms, Sorted, SortedLike, toNewSeq}
import strawman.collection.mutable.Builder

import scala.annotation.unchecked.uncheckedVariance
import scala.Ordering

trait SortedMap[K, +V]
  extends Map[K, V]
    with Sorted[K]
    with SortedMapLike[K, V, SortedMap]

trait SortedMapLike[K, +V, +C[X, +Y] <: SortedMap[X, Y]]
  extends SortedLike[K, C[K, V]]
    with SortedMapPolyTransforms[K, V, C]
    with MapLike[K, V, Map] // Inherited Map operations can only return a `Map` because they don’t take an evidence `Ordering`
    with MapMonoTransforms[K, V, C[K, V]] { // Operations that return the same collection type can return a `SortedMap`, though

  def firstKey: K = head._1

  def lastKey: K = last._1

}

/** Polymorphic transformation methods for sorted Maps */
trait SortedMapPolyTransforms[K, +V, +C[X, Y] <: Sorted[X]]
  // We inherit polymorphic transformations returning an Iterable (e.g. to
  // support the following use case `kvs.map((k, v) => v)`)
  extends IterablePolyTransforms[(K, V), Iterable]
  // Then we also inherit polymorphic transformations returning a Map, just
  // to get inheritance linearization right and disambiguate between
  // overloaded methods
    with MapPolyTransforms[K, V, Map] {

  // And finally, we add new overloads taking an ordering
  def map[K2, V2](f: (K, V) => (K2, V2))(implicit ordering: Ordering[K2]): C[K2, V2]

  /**
    * Add a key/value pair to this map, returning a new map.
    *
    * @param kv the key/value pair.
    * @tparam V1 the type of the value in the key/value pair.
    * @return A new map with the new binding added to this map.
    */
  def + [V1 >: V](kv: (K, V1)): C[K, V1]

}
