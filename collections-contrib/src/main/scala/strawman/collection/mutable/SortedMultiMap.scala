package strawman
package collection
package mutable

import collection.decorators._

/**
  * A mutable multimap whose keys are sorted
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class SortedMultiMap[K, V] private (elems: SortedMap[K, Set[V]])(implicit val ordering: Ordering[K])
  extends collection.SortedMultiMap[K, V]
    with collection.SortedMultiMapOps[K, V, SortedMultiMap, SortedMultiMap[K, V]]
    with Growable[(K, V)]
    with Shrinkable[(K, V)] {

  def sets: collection.SortedMap[K, collection.Set[V]] = elems

  def iterableFactory: IterableFactoryLike[collection.Iterable] = collection.Iterable
  def sortedMultiMapFactory: SortedMapFactory[SortedMultiMap] = SortedMultiMap

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): SortedMultiMap[K, V] = sortedMultiMapFactory.from(coll)
  protected[this] def sortedFromIterable[L: Ordering, W](it: collection.Iterable[(L, W)]): SortedMultiMap[L, W] = sortedMultiMapFactory.from(it)
  protected[this] def newSpecificBuilder(): Builder[(K, V), SortedMultiMap[K, V]] = sortedMultiMapFactory.newBuilder()

  def rangeImpl(from: Option[K], until: Option[K]): SortedMultiMap[K, V] =
    new SortedMultiMap(elems.rangeImpl(from, until))

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

object SortedMultiMap extends SortedMapFactory[SortedMultiMap] {

  def empty[K: Ordering, V]: SortedMultiMap[K, V] =
    new SortedMultiMap(SortedMap.empty)

  def from[K: Ordering, V](it: IterableOnce[(K, V)]): SortedMultiMap[K, V] =
    (newBuilder[K, V]() ++= it).result()

  def newBuilder[K: Ordering, V](): Builder[(K, V), SortedMultiMap[K, V]] =
    new GrowableBuilder[(K, V), SortedMultiMap[K, V]](empty)

}