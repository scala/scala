package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.{Builder, ImmutableBuilder}
import scala.language.higherKinds


/** Base type of immutable Maps */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapOps[K, V, Map, Map[K, V]] {

  override def mapFactory: scala.collection.MapFactory[MapCC] = Map

  override final def toMap[K2, V2](implicit ev: (K, V) <:< (K2, V2)): Map[K2, V2] = this.asInstanceOf[Map[K2, V2]]

  /** The same map with a given default function.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefault`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     the function mapping keys to values, used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefault[V1 >: V](d: K => V1): Map.WithDefault[K, V1] = new Map.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     default value used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefaultValue[V1 >: V](d: V1): Map.WithDefault[K, V1] = new Map.WithDefault[K, V1](this, _ => d)
}

/** Base trait of immutable Maps implementations
  *
  * @define coll immutable map
  * @define Coll `immutable.Map`
  */
trait MapOps[K, +V, +CC[X, +Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C] {

  protected def coll: C with CC[K, V]

  /** Removes a key from this map, returning a new map.
    *
    * @param key the key to be removed
    * @return a new map without a binding for ''key''
    */
  def remove(key: K): C

  /** Alias for `remove` */
  @deprecatedOverriding("This method should be final, but is not due to scala/bug#10853", "2.13.0")
  /*@`inline` final*/ def - (key: K): C = remove(key)

  @deprecated("Use -- with an explicit collection", "2.13.0")
  def - (key1: K, key2: K, keys: K*): C = remove(key1).remove(key2).removeAll(keys)

  /** Creates a new $coll from this $coll by removing all elements of another
    *  collection.
    *
    *  @param keys   the collection containing the removed elements.
    *  @return a new $coll that contains all elements of the current $coll
    *  except one less occurrence of each of the elements of `elems`.
    */
  def removeAll(keys: IterableOnce[K]): C = keys.iterator.foldLeft[C](coll)(_ - _)

  /** Alias for `removeAll` */
  @deprecatedOverriding("This method should be final, but is not due to scala/bug#10853", "2.13.0")
  /* @`inline` final */ override def -- (keys: IterableOnce[K]): C = removeAll(keys)

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
  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)

  /** This function transforms all the values of mappings contained
    *  in this map with function `f`.
    *
    *  @param f A function over keys and values
    *  @return  the updated map
    */
  def transform[W](f: (K, V) => W): CC[K, W] = map { case (k, v) => (k, f(k, v)) }

  override def concat [V1 >: V](that: collection.IterableOnce[(K, V1)]): CC[K, V1] = {
    var result: CC[K, V1] = coll
    val it = that.iterator
    while (it.hasNext) result = result + it.next()
    result
  }

  override def keySet: Set[K] = new ImmutableKeySet

  /** The implementation class of the set returned by `keySet` */
  protected class ImmutableKeySet extends AbstractSet[K] with GenKeySet {
    def incl(elem: K): Set[K] = if (this(elem)) this else empty ++ this + elem
    def excl(elem: K): Set[K] = if (this(elem)) empty ++ this - elem else this
  }

}

/**
  * $factoryInfo
  * @define coll immutable map
  * @define Coll `immutable.Map`
  */
@SerialVersionUID(3L)
object Map extends MapFactory[Map] {

  // getenv not getProperty for Scala.js friendliness.
  // TODO remove before 2.13.0-RC1? see scala/collection-strawman#572
  private final val useBaseline: Boolean =
    System.getenv("SCALA_COLLECTION_IMMUTABLE_USE_BASELINE") == "true"

  class WithDefault[K, +V](val underlying: Map[K, V], val defaultValue: K => V)
    extends AbstractMap[K, V]
      with MapOps[K, V, Map, WithDefault[K, V]] {

    def get(key: K): Option[V] = underlying.get(key)

    override def default(key: K): V = defaultValue(key)

    override def iterableFactory: IterableFactory[Iterable] = underlying.iterableFactory

    def iterator: Iterator[(K, V)] = underlying.iterator

    override def isEmpty: Boolean = underlying.isEmpty

    override def mapFactory: MapFactory[Map] = underlying.mapFactory

    override def concat [V2 >: V](xs: collection.IterableOnce[(K, V2)]): WithDefault[K, V2] =
      new WithDefault(underlying.concat(xs), defaultValue)

    def remove(key: K): WithDefault[K, V] = new WithDefault[K, V](underlying.remove(key), defaultValue)

    def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] =
      new WithDefault[K, V1](underlying.updated(key, value), defaultValue)

    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    override protected def fromSpecific(coll: collection.IterableOnce[(K, V)] @uncheckedVariance): WithDefault[K, V] =
      new WithDefault[K, V](mapFactory.from(coll), defaultValue)

    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] @uncheckedVariance =
      Map.newBuilder.mapResult((p: Map[K, V]) => new WithDefault[K, V](p, defaultValue))
  }

  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): Map[K, V] =
    it match {
      case it: Iterable[_] if it.isEmpty => empty[K, V]
      case m: Map[K, V] => m
      case _ => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: Builder[(K, V), Map[K, V]] =
    new ImmutableBuilder[(K, V), Map[K, V]](empty) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

  private object EmptyMap extends AbstractMap[Any, Nothing] {
    override def size: Int = 0
    override def knownSize: Int = 0
    override def isEmpty: Boolean = true
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def remove(key: Any): Map[Any, Nothing] = this
  }

  final class Map1[K, +V](key1: K, value1: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] {
    override def size: Int = 1
    override def knownSize: Int = 1
    override def isEmpty: Boolean = false
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    def iterator = Iterator.single((key1, value1))
    override def keysIterator: Iterator[K] = Iterator.single(key1)
    override def valuesIterator: Iterator[V] = Iterator.single(value1)
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def remove(key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
  }

  final class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] {
    override def size: Int = 2
    override def knownSize: Int = 2
    override def isEmpty: Boolean = false
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else default
    def iterator: Iterator[(K, V)] = new Map2Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    override def keysIterator: Iterator[K] = new Map2Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    override def valuesIterator: Iterator[V] = new Map2Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map2Iterator[A] extends AbstractIterator[A] {
      var i = 0
      override def hasNext: Boolean = i < 2
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
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

  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] {
    override def size: Int = 3
    override def knownSize: Int = 3
    override def isEmpty: Boolean = false
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
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else default
    def iterator: Iterator[(K, V)] = new Map3Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    override def keysIterator: Iterator[K] = new Map3Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    override def valuesIterator: Iterator[V] = new Map3Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map3Iterator[A] extends AbstractIterator[A] {
      var i = 0
      override def hasNext: Boolean = i < 3
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case 2 => nextResult(key3, value3)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
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

  final class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] {
    override def size: Int = 4
    override def knownSize: Int = 4
    override def isEmpty: Boolean = false
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
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else default
    def iterator: Iterator[(K, V)] = new Map4Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    override def keysIterator: Iterator[K] = new Map4Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    override def valuesIterator: Iterator[V] = new Map4Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map4Iterator[A] extends AbstractIterator[A] {
      var i = 0
      override def hasNext: Boolean = i < 4
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case 2 => nextResult(key3, value3)
          case 3 => nextResult(key4, value4)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else (if (useBaseline) OldHashMap.empty[K, V1] else HashMap.empty[K, V1]).updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
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

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
