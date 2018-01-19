package strawman
package collection
package mutable

import collection.decorators._

/**
  * A mutable multimap
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class MultiMap[K, V] private (elems: Map[K, Set[V]])
  extends collection.MultiMap[K, V]
    with collection.MultiMapOps[K, V, MultiMap, MultiMap[K, V]]
    with Growable[(K, V)]
    with Shrinkable[(K, V)] {

  def iterableFactory: IterableFactory[collection.Iterable] = collection.Iterable
  def multiMapFactory: MapFactory[MultiMap] = MultiMap
  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): MultiMap[K, V] = multiMapFactory.from(coll)
  protected[this] def newSpecificBuilder(): Builder[(K, V), MultiMap[K, V]] = multiMapFactory.newBuilder()

  def sets: collection.Map[K, collection.Set[V]] = elems

  def addOne(elem: (K, V)): this.type = {
    val (k, v) = elem
    elems.updateWith(k) {
      case None     => Some(Set(v))
      case Some(vs) => Some(vs += v)
    }
    this
  }

  def subtractOne(elem: (K, V)): this.type = {
    val (k, v) = elem
    elems.updateWith(k) {
      case Some(vs) =>
        vs -= v
        if (vs.nonEmpty) Some(vs) else None
    }
    this
  }

  /**
    * Removes all the entries associated with the given `key`
    * @return the collection itself
    */
  def removeKey(key: K): this.type = {
    elems -= key
    this
  }

  /** Alias for `removeKey` */
  @`inline` final def -*= (key: K): this.type = removeKey(key)

  def clear(): Unit = elems.clear()

}

object MultiMap extends MapFactory[MultiMap] {

  def empty[K, V]: MultiMap[K, V] = new MultiMap(Map.empty)

  def from[K, V](source: IterableOnce[(K, V)]): MultiMap[K, V] = (newBuilder[K, V]() ++= source).result()

  def newBuilder[K, V](): Builder[(K, V), MultiMap[K, V]] = new GrowableBuilder[(K, V), MultiMap[K, V]](empty)

}