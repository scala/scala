package strawman
package collection
package immutable

import strawman.collection.mutable.Builder

import scala.{Any, Boolean, `inline`, Int, None, NoSuchElementException, Nothing, Option, Some, Serializable, Unit}

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
    def incl(elem: K): Set[K] = if (this(elem)) this else empty ++ this + elem
    def excl(elem: K): Set[K] = if (this(elem)) empty ++ this - elem else this
  }

}

object Map extends MapFactory[Map] {

  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  def fromIterable[K, V](it: collection.Iterable[(K, V)]): Map[K, V] =
    it match {
      case m: Map[K, V] => m
      case _ => empty ++ it
    }

  def newBuilder[K, V](): Builder[(K, V), Map[K, V]] = HashMap.newBuilder()

  trait SmallMap[K, +V] extends Map[K, V] {
    def iterableFactory: IterableFactory[Iterable] = Iterable
    def mapFactory: MapFactory[Map] = Map
    def empty: Map[K, V] = mapFactory.empty
    protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): Map[K, V] = mapFactory.fromIterable(coll)
    protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] = mapFactory.fromIterable(it)
    protected[this] def newSpecificBuilder(): Builder[(K, V), Map[K, V]] = mapFactory.newBuilder()
  }

  private object EmptyMap extends Map[Any, Nothing] with SmallMap[Any, Nothing] with Serializable {
    override def size: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    def iterator(): Iterator[(Any, Nothing)] = Iterator.empty
    def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def remove(key: Any): Map[Any, Nothing] = this
  }

  final class Map1[K, +V](key1: K, value1: V) extends Map[K, V] with SmallMap[K, V] with Serializable {
    override def size = 1
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    def iterator() = Iterator.single((key1, value1))
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def remove(key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
  }

  final class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends Map[K, V] with SmallMap[K, V] with Serializable {
    override def size = 2
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    def iterator() = ((key1, value1) :: (key2, value2) :: Nil).iterator()
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def remove(key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
  }

  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends Map[K, V] with SmallMap[K, V] with Serializable {
    override def size = 3
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    def iterator() = ((key1, value1) :: (key2, value2) :: (key3, value3) :: Nil).iterator()
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def remove(key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
  }

  final class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends Map[K, V] with SmallMap[K, V] with Serializable {
    override def size = 4
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3) || (key == key4)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    def iterator() = ((key1, value1) :: (key2, value2) :: (key3, value3) :: (key4, value4) :: Nil).iterator()
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else HashMap.empty.updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
    def remove(key: K): Map[K, V] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
  }

}
