package strawman
package collection

import strawman.collection.mutable.Builder

import scala.Ordering

/** Base type of sorted sets */
trait SortedMap[K, +V]
  extends Map[K, V]
     with Sorted[K]
     with SortedMapLike[K, V, SortedMap]

trait SortedMapLike[K, +V, +CC[X, +Y] <: SortedMap[X, Y] with SortedMapLike[X, Y, CC]]
 extends SortedOps[K, CC[K, V]]
    with MapLike[K, V, Map]
    with MapOps[K, V, CC[K, V]]
    with SortedMapMappings[K, V, CC] {
  def firstKey: K = head._1
  def lastKey: K = last._1

  def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]

}

/** Polymorphic transformation methods for sorted Maps */
trait SortedMapMappings[K, +V, +CC[X, +Y] <: SortedMap[X, Y] with SortedMapLike[X, Y, CC]]
  // We inherit polymorphic transformations returning an Iterable (e.g. to
  // support the following use case `kvs.map((k, v) => v)`)
  extends MapMappings[K, V, Map] {

  protected def coll: CC[K, V]

  def orderedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2]

  // And finally, we add new overloads taking an ordering
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit ordering: Ordering[K2]): CC[K2, V2] =
    orderedMapFromIterable(View.Map[(K, V), (K2, V2)](coll, f))

  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    orderedMapFromIterable(View.FlatMap(coll, f))

  def ++[K2 >: K, V2 >: V](xs: IterableOnce[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    orderedMapFromIterable(View.Concat(coll, xs))
}

object SortedMap extends OrderedSetFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = immutable.SortedSet.empty
  def orderedNewBuilder[A : Ordering]: Builder[A, SortedSet[A]] = immutable.SortedSet.orderedNewBuilder
  def orderedFromIterable[E : Ordering](it: Iterable[E]): SortedSet[E] = immutable.SortedSet.orderedFromIterable(it)
}

