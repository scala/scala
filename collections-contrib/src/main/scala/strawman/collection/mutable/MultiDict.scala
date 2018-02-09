package strawman
package collection
package mutable

import collection.decorators._

/**
  * A mutable multidict
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class MultiDict[K, V] private (elems: Map[K, Set[V]])
  extends collection.MultiDict[K, V]
    with collection.MultiDictOps[K, V, MultiDict, MultiDict[K, V]]
    with Growable[(K, V)]
    with Shrinkable[(K, V)] {

  def iterableFactory: IterableFactory[collection.Iterable] = collection.Iterable
  def multiMapFactory: MapFactory[MultiDict] = MultiDict
  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): MultiDict[K, V] = multiMapFactory.from(coll)
  protected[this] def newSpecificBuilder(): Builder[(K, V), MultiDict[K, V]] = multiMapFactory.newBuilder()

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

object MultiDict extends MapFactory[MultiDict] {

  def empty[K, V]: MultiDict[K, V] = new MultiDict(Map.empty)

  def from[K, V](source: IterableOnce[(K, V)]): MultiDict[K, V] = (newBuilder[K, V]() ++= source).result()

  def newBuilder[K, V](): Builder[(K, V), MultiDict[K, V]] = new GrowableBuilder[(K, V), MultiDict[K, V]](empty)

}