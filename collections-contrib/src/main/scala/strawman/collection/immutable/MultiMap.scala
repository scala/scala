package strawman
package collection
package immutable

import collection.decorators._
import strawman.collection.mutable.{Builder, ImmutableBuilder}

/**
  * An immutable multimap
  * @tparam K the type of keys
  * @tparam V the type of values
  */
class MultiMap[K, V] private (elems: Map[K, Set[V]])
  extends collection.MultiMap[K, V]
    with Iterable[(K, V)]
    with collection.MultiMapOps[K, V, MultiMap, MultiMap[K, V]]
    with collection.IterableOps[(K, V), Iterable, MultiMap[K, V]] {

  def sets: Map[K, Set[V]] = elems

  def iterableFactory: IterableFactory[Iterable] = Iterable
  def multiMapFactory: MapFactory[MultiMap] = MultiMap

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): MultiMap[K, V] = multiMapFromIterable(coll)
  protected[this] def newSpecificBuilder(): Builder[(K, V), MultiMap[K, V]] = multiMapFactory.newBuilder[K, V]()

  /**
    * @return a new multimap that contains all the entries of this multimap
    *         excepted the entry defined by the given `key` and `value`
    */
  def remove(key: K, value: V): MultiMap[K, V] =
    new MultiMap(elems.updatedWith(key) {
      case Some(vs) =>
        val updatedVs = vs - value
        if (updatedVs.nonEmpty) Some(updatedVs) else None
    })

  /** Alias for `remove` */
  @`inline` final def - (kv: (K, V)): MultiMap[K, V] = remove(kv._1, kv._2)

  /**
    * @return a new multimap that contains all the entries of this multimap
    *         excepted those associated with the given `key`
    */
  def removeKey(key: K): MultiMap[K, V] = new MultiMap(elems - key)

  /** Alias for `removeKey` */
  @`inline` final def -* (key: K): MultiMap[K, V] = removeKey(key)

  /**
    * @return a new multimap that contains all the entries of this multimap
    *         and the entry defined by the given `key` and `value`
    */
  def add(key: K, value: V): MultiMap[K, V] =
    new MultiMap(elems.updatedWith(key) {
      case None     => Some(Set(value))
      case Some(vs) => Some(vs + value)
    })

  /** Alias for `add` */
  @`inline` final def + (kv: (K, V)): MultiMap[K, V] = add(kv._1, kv._2)

}

object MultiMap extends MapFactory[MultiMap] {

  def empty[K, V]: MultiMap[K, V] = new MultiMap[K, V](Map.empty)

  def from[K, V](source: IterableOnce[(K, V)]): MultiMap[K, V] =
    source match {
      case mm: MultiMap[K, V] => mm
      case _ => (newBuilder[K, V]() ++= source).result()
    }

  def newBuilder[K, V](): Builder[(K, V), MultiMap[K, V]] =
    new ImmutableBuilder[(K, V), MultiMap[K, V]](empty[K, V]) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

}