package strawman
package collection
package immutable

import scala.Ordering

trait SortedMap[K, +V]
  extends Map[K, V]
     with collection.SortedMap[K, V]
     with SortedMapLike[K, V, SortedMap]

trait SortedMapLike[K, +V, +CC[X, +Y] <: SortedMap[X, Y] with SortedMapLike[X, Y, CC]]
  extends MapLike[K, V, Map]
     with collection.SortedMapLike[K, V, CC] {

  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]
}

