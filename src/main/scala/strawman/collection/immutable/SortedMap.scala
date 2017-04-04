package strawman
package collection.immutable

import strawman.collection.{IterablePolyTransforms, Sorted, SortedLike, View}

import scala.Ordering

trait SortedMap[K, +V]
  extends Map[K, V]
    with Sorted[K]
    with SortedMapLike[K, V, SortedMap]

trait SortedMapLike[K, +V, +C[X, +Y] <: SortedMap[X, Y] with SortedMapLike[X, Y, C]]
  extends SortedLike[K, C[K, V]]
    with MapLike[K, V, Map] // Inherited Map operations can only return a `Map` because they don’t take an evidence `Ordering`
    with MapMonoTransforms[K, V, C[K, V]] // Operations that return the same collection type can return a `SortedMap`, though
    with SortedMapPolyTransforms[K, V, C] {

  protected def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] =
    Map.fromIterable(it)

  def firstKey: K = head._1

  def lastKey: K = last._1

}

/** Polymorphic transformation methods for sorted Maps */
trait SortedMapPolyTransforms[K, +V, +C[X, +Y] <: SortedMap[X, Y] with SortedMapLike[X, Y, C]]
  // We inherit polymorphic transformations returning an Iterable (e.g. to
  // support the following use case `kvs.map((k, v) => v)`)
  extends IterablePolyTransforms[(K, V), Iterable]
    // Then we also inherit polymorphic transformations returning a Map, just
    // to get inheritance linearization right and disambiguate between
    // overloaded methods
    with MapPolyTransforms[K, V, Map]
    // Last, we still want operations that are polymorphic only in the Map values
    // to return a sorted map
    with MapValuePolyTransforms[K, V, C] {

  protected def coll: C[K, V]

  def constrainedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): C[K2, V2]

  // And finally, we add new overloads taking an ordering
  def map[K2, V2](f: (K, V) => (K2, V2))(implicit ordering: Ordering[K2]): C[K2, V2] =
    constrainedMapFromIterable(View.Map(coll, f.tupled))

}
