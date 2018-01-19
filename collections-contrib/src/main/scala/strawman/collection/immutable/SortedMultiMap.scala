package strawman
package collection
package immutable

import collection.mutable.{Builder, ImmutableBuilder}
import collection.decorators._

/**
  * An immutable multimap whose keys are sorted
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class SortedMultiMap[K, V] private (elems: SortedMap[K, Set[V]])(implicit val ordering: Ordering[K])
  extends collection.SortedMultiMap[K, V]
    with Iterable[(K, V)]
    with collection.SortedMultiMapOps[K, V, SortedMultiMap, SortedMultiMap[K, V]]
    with collection.IterableOps[(K, V), Iterable, SortedMultiMap[K, V]] {

  def sortedMultiMapFactory: SortedMapFactory[SortedMultiMap] = SortedMultiMap
  def iterableFactory: IterableFactory[Iterable] = Iterable

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): SortedMultiMap[K, V] = sortedMultiMapFactory.from(coll)
  protected[this] def sortedFromIterable[L: Ordering, W](it: collection.Iterable[(L, W)]): SortedMultiMap[L, W] = sortedMultiMapFactory.from(it)
  protected[this] def newSpecificBuilder(): Builder[(K, V), SortedMultiMap[K, V]] = sortedMultiMapFactory.newBuilder()

  def sets: SortedMap[K, Set[V]] = elems

  def rangeImpl(from: Option[K], until: Option[K]): SortedMultiMap[K, V] =
    new SortedMultiMap(elems.rangeImpl(from, until))

  /**
    * @return a new sorted multimap that contains all the entries of this sorted multimap
    *         and the entry defined by the given `key` and `value`
    */
  def add(key: K, value: V): SortedMultiMap[K, V] =
    new SortedMultiMap(elems.updatedWith(key) {
      case None     => Some(Set(value))
      case Some(vs) => Some(vs + value)
    })

  /** Alias for `add` */
  @`inline` final def + (kv: (K, V)): SortedMultiMap[K, V] = add(kv._1, kv._2)

  /**
    * @return a new multimap that contains all the entries of this multimap
    *         excepted the entry defined by the given `key` and `value`
    */
  def remove(key: K, value: V): SortedMultiMap[K, V] =
    new SortedMultiMap(elems.updatedWith(key) {
      case Some(vs) =>
        val updatedVs = vs - value
        if (updatedVs.nonEmpty) Some(updatedVs) else None
    })

  /** Alias for `remove` */
  @`inline` final def - (kv: (K, V)): SortedMultiMap[K, V] = remove(kv._1, kv._2)

  /**
    * @return a new multimap that contains all the entries of this multimap
    *         excepted those associated with the given `key`
    */
  def removeKey(key: K): SortedMultiMap[K, V] = new SortedMultiMap(elems - key)

  /** Alias for `removeKey` */
  @`inline` final def -* (key: K): SortedMultiMap[K, V] = removeKey(key)

}

object SortedMultiMap extends SortedMapFactory[SortedMultiMap] {

  def empty[K: Ordering, V]: SortedMultiMap[K, V] = new SortedMultiMap[K, V](SortedMap.empty[K, Set[V]])

  def from[K: Ordering, V](it: IterableOnce[(K, V)]): SortedMultiMap[K, V] =
    it match {
      case smm: SortedMultiMap[K, V] => smm
      case _ => (newBuilder[K, V]() ++= it).result()
    }

  def newBuilder[K: Ordering, V](): Builder[(K, V), SortedMultiMap[K, V]] =
    new ImmutableBuilder[(K, V), SortedMultiMap[K, V]](empty[K, V]) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

}