package strawman
package collection
package mutable

import collection.decorators._

/**
  * A mutable multidict whose keys are sorted
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class SortedMultiDict[K, V] private (elems: SortedMap[K, Set[V]])(implicit val ordering: Ordering[K])
  extends collection.SortedMultiDict[K, V]
    with collection.SortedMultiDictOps[K, V, SortedMultiDict, SortedMultiDict[K, V]]
    with Growable[(K, V)]
    with Shrinkable[(K, V)] {

  def sets: collection.SortedMap[K, collection.Set[V]] = elems

  def iterableFactory: IterableFactory[collection.Iterable] = collection.Iterable
  def sortedMultiMapFactory: SortedMapFactory[SortedMultiDict] = SortedMultiDict

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): SortedMultiDict[K, V] = sortedMultiMapFactory.from(coll)
  protected[this] def sortedFromIterable[L: Ordering, W](it: collection.Iterable[(L, W)]): SortedMultiDict[L, W] = sortedMultiMapFactory.from(it)
  protected[this] def newSpecificBuilder(): Builder[(K, V), SortedMultiDict[K, V]] = sortedMultiMapFactory.newBuilder()

  def rangeImpl(from: Option[K], until: Option[K]): SortedMultiDict[K, V] =
    new SortedMultiDict(elems.rangeImpl(from, until))

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

object SortedMultiDict extends SortedMapFactory[SortedMultiDict] {

  def empty[K: Ordering, V]: SortedMultiDict[K, V] =
    new SortedMultiDict(SortedMap.empty)

  def from[K: Ordering, V](it: IterableOnce[(K, V)]): SortedMultiDict[K, V] =
    (newBuilder[K, V]() ++= it).result()

  def newBuilder[K: Ordering, V](): Builder[(K, V), SortedMultiDict[K, V]] =
    new GrowableBuilder[(K, V), SortedMultiDict[K, V]](empty)

}