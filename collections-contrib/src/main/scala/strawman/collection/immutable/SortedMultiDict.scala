package strawman
package collection
package immutable

import collection.mutable.{Builder, ImmutableBuilder}
import collection.decorators._

/**
  * An immutable multidict whose keys are sorted
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class SortedMultiDict[K, V] private (elems: SortedMap[K, Set[V]])(implicit val ordering: Ordering[K])
  extends collection.SortedMultiDict[K, V]
    with Iterable[(K, V)]
    with collection.SortedMultiDictOps[K, V, SortedMultiDict, SortedMultiDict[K, V]]
    with collection.IterableOps[(K, V), Iterable, SortedMultiDict[K, V]] {

  def sortedMultiMapFactory: SortedMapFactory[SortedMultiDict] = SortedMultiDict

  protected[this] def sortedFromIterable[L: Ordering, W](it: collection.Iterable[(L, W)]): SortedMultiDict[L, W] = sortedMultiMapFactory.from(it)

  def sets: SortedMap[K, Set[V]] = elems

  def rangeImpl(from: Option[K], until: Option[K]): SortedMultiDict[K, V] =
    new SortedMultiDict(elems.rangeImpl(from, until))

  /**
    * @return a new sorted multidict that contains all the entries of this sorted multidict
    *         and the entry defined by the given `key` and `value`
    */
  def add(key: K, value: V): SortedMultiDict[K, V] =
    new SortedMultiDict(elems.updatedWith(key) {
      case None     => Some(Set(value))
      case Some(vs) => Some(vs + value)
    })

  /** Alias for `add` */
  @`inline` final def + (kv: (K, V)): SortedMultiDict[K, V] = add(kv._1, kv._2)

  /**
    * @return a new multidict that contains all the entries of this multidict
    *         excepted the entry defined by the given `key` and `value`
    */
  def remove(key: K, value: V): SortedMultiDict[K, V] =
    new SortedMultiDict(elems.updatedWith(key) {
      case Some(vs) =>
        val updatedVs = vs - value
        if (updatedVs.nonEmpty) Some(updatedVs) else None
    })

  /** Alias for `remove` */
  @`inline` final def - (kv: (K, V)): SortedMultiDict[K, V] = remove(kv._1, kv._2)

  /**
    * @return a new multidict that contains all the entries of this multidict
    *         excepted those associated with the given `key`
    */
  def removeKey(key: K): SortedMultiDict[K, V] = new SortedMultiDict(elems - key)

  /** Alias for `removeKey` */
  @`inline` final def -* (key: K): SortedMultiDict[K, V] = removeKey(key)

}

object SortedMultiDict extends SortedMapFactory[SortedMultiDict] {

  def empty[K: Ordering, V]: SortedMultiDict[K, V] = new SortedMultiDict[K, V](SortedMap.empty[K, Set[V]])

  def from[K: Ordering, V](it: IterableOnce[(K, V)]): SortedMultiDict[K, V] =
    it match {
      case smm: SortedMultiDict[K, V] => smm
      case _ => (newBuilder[K, V]() ++= it).result()
    }

  def newBuilder[K: Ordering, V](): Builder[(K, V), SortedMultiDict[K, V]] =
    new ImmutableBuilder[(K, V), SortedMultiDict[K, V]](empty[K, V]) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

}