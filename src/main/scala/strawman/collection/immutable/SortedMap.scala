package strawman
package collection.immutable

import strawman.collection.{IterableMonoTransforms, IterablePolyTransforms, MapPolyTransforms, Sorted, SortedLike}

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
    with MapOps[K, V, C] // Operations that return the same collection type can return a `SortedMap`, though
    with IterableMonoTransforms[(K, V), C[K, V]] // Transformation operations that return the same collection type can return a `SortedMap`, though

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

}

