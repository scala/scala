package strawman.collection.mutable

import strawman.collection.{IterableOnce, Iterator, MapFactories}

import scala.{Option, Unit}
import scala.Predef.???

/** A mutable map backed by a hashtable */
final class HashMap[K, V]
  extends Map[K, V]
    with MapLike[K, V, HashMap] {

  // From IterableOnce
  def iterator(): Iterator[(K, V)] = ???

  // From MapLike
  def get(key: K): Option[V] = ???

  // From Growable
  def +=(elem: (K, V)): this.type = ???
  def clear(): Unit = ???

  // From mutable.MapLike
  def -=(elem: (K, V)): this.type = ???
  def put(key: K, value: V): Option[V] = ???

  // From MapPolyTransforms
  def map[K2, V2](f: (K, V) => (K2, V2)): HashMap[K2, V2] = ???
  def flatMap[K2, V2](f: (K, V) => IterableOnce[(K2, V2)]): HashMap[K2, V2] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Iterable[B] = ???
  // From IterableMonoTransforms
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[(K, V)]): HashMap[K, V] = ???

}

object HashMap extends MapFactories[HashMap] {

  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = ???

}