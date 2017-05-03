package strawman
package collection.mutable

/**
  * Base type for mutable sorted map collections
  */
trait SortedMap[K, V]
  extends collection.SortedMap[K, V]
    with Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, V, +CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMap[K, V]]
  extends collection.SortedMapOps[K, V, CC, C]
    with MapOps[K, V, Map, C] {

  def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] = Map.fromIterable(it)

}
