package strawman
package collection
package immutable

import scala.Ordering

trait SortedMap[K, +V]
  extends Map[K, V]
     with collection.SortedMap[K, V]
     with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, +V, +CC[X, +Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMap[K, V]]
  extends MapOps[K, V, Map, C]
     with collection.SortedMapOps[K, V, CC, C] {

    protected def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] =
    Map.fromIterable(it)

}

