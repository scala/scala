package strawman
package collection.immutable

import strawman.collection.{ConstrainedMapFactory, SortedLike}
import strawman.collection.mutable.Builder

import scala.{Option, Ordering}
import scala.Predef.???

final class TreeMap[K, +V]
  extends SortedMap[K, V]
    with SortedMapLike[K, V, TreeMap] {

  // Members declared in collection.IterableLike
  protected[this] def fromIterableWithSameElemType(coll: collection.Iterable[(K, V)]): TreeMap[K, V] = ???

  // Members declared in collection.IterableOnce
  def iterator(): collection.Iterator[(K, V)] = ???

  // Members declared in collection.IterablePolyTransforms
  def fromIterable[B](coll: collection.Iterable[B]): collection.immutable.Iterable[B] = ???

  // Members declared in collection.MapLike
  def get(key: K): Option[V] = ???

  // Members declared in collection.immutable.MapMonoTransforms
  def -(key: K): TreeMap[K,V] = ???
  def +[V1 >: V](kv: (K, V1)): TreeMap[K,V1] = ???

  // Members declared in collection.MapPolyTransforms
  def flatMap[K2, V2](f: (K, V) => collection.IterableOnce[(K2, V2)]): collection.immutable.Map[K2,V2] = ???
  def map[K2, V2](f: (K, V) => (K2, V2)): collection.immutable.Map[K2,V2] = ???

  // Members declared in collection.SortedMapPolyTransforms
  def map[K2, V2](f: (K, V) => (K2, V2))(implicit ordering: Ordering[K2]): collection.immutable.TreeMap[K2,V2] = ???

  // Members declared in collection.SortedLike
  def ordering: Ordering[K] = ???
  def range(from: K,until: K): TreeMap[K,V] = ???

}

object TreeMap extends ConstrainedMapFactory[TreeMap, Ordering] {

  def constrainedNewBuilder[K : Ordering, V]: Builder[(K, V), TreeMap[K, V]] = ???

  def empty[K: Ordering, V]: TreeMap[K, V] = ???

}
