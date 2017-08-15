package strawman
package collection.mutable

/**
  * Base type for mutable sorted map collections
  */
trait SortedMap[K, V]
  extends collection.SortedMap[K, V]
    with Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends collection.SortedMapOps[K, V, CC, C]
    with MapOps[K, V, Map, C] {

  def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] = Map.fromIterable(it)

}
