package strawman
package collection
package immutable

import strawman.collection.mutable.Builder

import scala.{Boolean, `inline`, Int, Serializable}

/** Base type of immutable Maps */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapOps[K, V, Map, Map[K, V]]

/** Base trait of immutable Maps implementations */
trait MapOps[K, +V, +CC[X, +Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C] {

  protected[this] def coll: C with CC[K, V]

  /** Removes a key from this map, returning a new map.
    *
    * @param key the key to be removed
    * @return a new map without a binding for ''key''
    */
  def remove(key: K): C

  /** Alias for `remove` */
  @`inline` final def - (key: K): C = remove(key)

  /** Creates a new $coll from this $coll by removing all elements of another
    *  collection.
    *
    *  @param keys   the collection containing the removed elements.
    *  @return a new $coll that contains all elements of the current $coll
    *  except one less occurrence of each of the elements of `elems`.
    */
  def removeAll(keys: IterableOnce[K]): C = keys.iterator().foldLeft[C](coll)(_ - _)

  /** Alias for `removeAll` */
  @`inline` final def -- (keys: IterableOnce[K]): C = removeAll(keys)

  /** Creates a new map obtained by updating this map with a given key/value pair.
    *  @param    key the key
    *  @param    value the value
    *  @tparam   V1 the type of the added value
    *  @return   A new map with the new key/value mapping added to this map.
    *
    *  @usecase  def updated(key: K, value: V): Map[K, V]
    *    @inheritdoc
    */
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]

  /**
    * Alias for `updated`
    *
    * @param kv the key/value pair.
    * @tparam V1 the type of the value in the key/value pair.
    * @return A new map with the new binding added to this map.
    */
  /*@`inline` final*/ def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)

  override def concat [V1 >: V](that: collection.Iterable[(K, V1)]): CC[K, V1] = {
    var result: CC[K, V1] = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

  override def keySet: Set[K] = new ImmutableKeySet

  /** The implementation class of the set returned by `keySet` */
  protected class ImmutableKeySet extends Set[K] with GenKeySet {
    def iterableFactory: IterableFactory[Set] = Set
    protected[this] def fromSpecificIterable(coll: collection.Iterable[K]): Set[K] = fromIterable(coll)
    protected[this] def newSpecificBuilder(): Builder[K, Set[K]] = iterableFactory.newBuilder()
    def empty: Set[K] = iterableFactory.empty
    def incl(elem: K): Set[K] = fromSpecificIterable(this).incl(elem)
    def excl(elem: K): Set[K] = fromSpecificIterable(this).excl(elem)
  }

}

// TODO Special case small maps
object Map extends MapFactory.Delegate[Map](HashMap)
