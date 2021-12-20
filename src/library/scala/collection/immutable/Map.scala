/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.Map.Map4
import scala.collection.mutable.{Builder, ReusableBuilder}

/** Base type of immutable Maps */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapOps[K, V, Map, Map[K, V]]
     with MapFactoryDefaults[K, V, Map, Iterable] {

  override def mapFactory: scala.collection.MapFactory[Map] = Map

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
  def withDefault[V1 >: V](d: K => V1): Map[K, V1] = new Map.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     default value used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefaultValue[V1 >: V](d: V1): Map[K, V1] = new Map.WithDefault[K, V1](this, _ => d)
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
  def removed(key: K): C

  /** Alias for `removed` */
  @`inline` final def - (key: K): C = removed(key)

  @deprecated("Use -- with an explicit collection", "2.13.0")
  def - (key1: K, key2: K, keys: K*): C = removed(key1).removed(key2).removedAll(keys)

  /** Creates a new $coll from this $coll by removing all elements of another
    *  collection.
    *
    *  $willForceEvaluation
    *
    *  @param keys   the collection containing the removed elements.
    *  @return a new $coll that contains all elements of the current $coll
    *  except one less occurrence of each of the elements of `elems`.
    */
  def removedAll(keys: IterableOnce[K]): C = keys.iterator.foldLeft[C](coll)(_ - _)

  /** Alias for `removedAll` */
  @`inline` final override def -- (keys: IterableOnce[K]): C = removedAll(keys)

  /** Creates a new map obtained by updating this map with a given key/value pair.
   *  @param    key the key
   *  @param    value the value
   *  @tparam   V1 the type of the added value
   *  @return   A new map with the new key/value mapping added to this map.
   */
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]

  /**
   * Update a mapping for the specified key and its current optionally-mapped value
   * (`Some` if there is current mapping, `None` if not).
   *
   * If the remapping function returns `Some(v)`, the mapping is updated with the new value `v`.
   * If the remapping function returns `None`, the mapping is removed (or remains absent if initially absent).
   * If the function itself throws an exception, the exception is rethrown, and the current mapping is left unchanged.
   *
   * @param key the key value
   * @param remappingFunction a partial function that receives current optionally-mapped value and return a new mapping
   * @return A new map with the updated mapping with the key
   */
  def updatedWith[V1 >: V](key: K)(remappingFunction: Option[V] => Option[V1]): CC[K,V1] = {
    val previousValue = this.get(key)
    val nextValue = remappingFunction(previousValue)
    (previousValue, nextValue) match {
      case (None, None) => coll
      case (Some(_), None) => this.removed(key).coll
      case (_, Some(v)) => this.updated(key, v)
    }
  }

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

  override def keySet: Set[K] = new ImmutableKeySet

  /** The implementation class of the set returned by `keySet` */
  protected class ImmutableKeySet extends AbstractSet[K] with GenKeySet with DefaultSerializable {
    def incl(elem: K): Set[K] = if (this(elem)) this else empty ++ this + elem
    def excl(elem: K): Set[K] = if (this(elem)) empty ++ this - elem else this
  }

}

trait StrictOptimizedMapOps[K, +V, +CC[X, +Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends MapOps[K, V, CC, C]
    with collection.StrictOptimizedMapOps[K, V, CC, C]
    with StrictOptimizedIterableOps[(K, V), Iterable, C] {

  override def concat [V1 >: V](that: collection.IterableOnce[(K, V1)]): CC[K, V1] = {
    var result: CC[K, V1] = coll
    val it = that.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}


/**
  * $factoryInfo
  * @define coll immutable map
  * @define Coll `immutable.Map`
  */
@SerialVersionUID(3L)
object Map extends MapFactory[Map] {

  @SerialVersionUID(3L)
  class WithDefault[K, +V](val underlying: Map[K, V], val defaultValue: K => V)
    extends AbstractMap[K, V]
      with MapOps[K, V, Map, WithDefault[K, V]] with Serializable {

    def get(key: K): Option[V] = underlying.get(key)

    override def default(key: K): V = defaultValue(key)

    override def iterableFactory: IterableFactory[Iterable] = underlying.iterableFactory

    def iterator: Iterator[(K, V)] = underlying.iterator

    override def isEmpty: Boolean = underlying.isEmpty

    override def mapFactory: MapFactory[Map] = underlying.mapFactory

    override def concat [V2 >: V](xs: collection.IterableOnce[(K, V2)]): WithDefault[K, V2] =
      new WithDefault(underlying.concat(xs), defaultValue)

    def removed(key: K): WithDefault[K, V] = new WithDefault[K, V](underlying.removed(key), defaultValue)

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

  def newBuilder[K, V]: Builder[(K, V), Map[K, V]] = new MapBuilderImpl

  @SerialVersionUID(3L)
  private object EmptyMap extends AbstractMap[Any, Nothing] with Serializable {
    override def size: Int = 0
    override def knownSize: Int = 0
    override def isEmpty: Boolean = true
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def keysIterator: Iterator[Any] = Iterator.empty
    override def valuesIterator: Iterator[Nothing] = Iterator.empty
    def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def removed(key: Any): Map[Any, Nothing] = this
    override def concat[V2 >: Nothing](suffix: IterableOnce[(Any, V2)]): Map[Any, V2] = suffix match {
      case m: immutable.Map[Any, V2] => m
      case _ => super.concat(suffix)
    }
  }

  @SerialVersionUID(3L)
  final class Map1[K, +V](key1: K, value1: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    override def size: Int = 1
    override def knownSize: Int = 1
    override def isEmpty: Boolean = false
    override def apply(key: K): V = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K): Boolean = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    def iterator: Iterator[(K, V)] = Iterator.single((key1, value1))
    override def keysIterator: Iterator[K] = Iterator.single(key1)
    override def valuesIterator: Iterator[V] = Iterator.single(value1)
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def removed(key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] =
      if (pred((key1, value1)) != isFlipped) this else Map.empty
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      if (walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) this.asInstanceOf[Map[K, W]]
      else new Map1(key1, walue1)
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 1
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(3L)
  final class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    override def size: Int = 2
    override def knownSize: Int = 2
    override def isEmpty: Boolean = false
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K): Boolean = (key == key1) || (key == key2)
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
      private[this] var i = 0
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
      override def drop(n: Int): Iterator[A] = { i += n; this }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def removed(key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2))
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1 = null.asInstanceOf[K]
      var v1 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             {k1 = key1; v1 = value1}; n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) {k1 = key2; v1 = value2}; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => this
      }
    }
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map2(key1, walue1, key2, walue2)
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 2
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(3L)
  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    override def size: Int = 3
    override def knownSize: Int = 3
    override def isEmpty: Boolean = false
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K): Boolean = (key == key1) || (key == key2) || (key == key3)
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
      private[this] var i = 0
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
      override def drop(n: Int): Iterator[A] = { i += n; this }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def removed(key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2)) || p((key3, value3))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2)) && p((key3, value3))
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1, k2 = null.asInstanceOf[K]
      var v1, v2 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             { k1 = key1; v1 = value1 };                                             n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) { k1 = key2; v1 = value2 } else             { k2 = key2; v2 = value2 }; n += 1}
      if (pred((key3, value3)) != isFlipped) { if (n == 0) { k1 = key3; v1 = value3 } else if (n == 1) { k2 = key3; v2 = value3 }; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => new Map2(k1, v1, k2, v2)
        case 3 => this
      }
    }
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      val walue3 = f(key3, value3)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) &&
          (walue3.asInstanceOf[AnyRef] eq value3.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map3(key1, walue1, key2, walue2, key3, walue3)
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 3
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key3, value3)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(3L)
  final class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V)
    extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {

    override def size: Int = 4
    override def knownSize: Int = 4
    override def isEmpty: Boolean = false
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K): Boolean = (key == key1) || (key == key2) || (key == key3) || (key == key4)
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
      private[this] var i = 0
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
      override def drop(n: Int): Iterator[A] = { i += n; this }
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else HashMap.empty[K, V1].updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
    def removed(key: K): Map[K, V] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2)) || p((key3, value3)) || p((key4, value4))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2)) && p((key3, value3)) && p((key4, value4))
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1, k2, k3 = null.asInstanceOf[K]
      var v1, v2, v3 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             { k1 = key1; v1 = value1 };                                                                                         n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) { k1 = key2; v1 = value2 } else             { k2 = key2; v2 = value2 };                                             n += 1}
      if (pred((key3, value3)) != isFlipped) { if (n == 0) { k1 = key3; v1 = value3 } else if (n == 1) { k2 = key3; v2 = value3 } else             { k3 = key3; v3 = value3};  n += 1}
      if (pred((key4, value4)) != isFlipped) { if (n == 0) { k1 = key4; v1 = value4 } else if (n == 1) { k2 = key4; v2 = value4 } else if (n == 2) { k3 = key4; v3 = value4 }; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => new Map2(k1, v1, k2, v2)
        case 3 => new Map3(k1, v1, k2, v2, k3, v3)
        case 4 => this
      }
    }
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      val walue3 = f(key3, value3)
      val walue4 = f(key4, value4)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) &&
          (walue3.asInstanceOf[AnyRef] eq value3.asInstanceOf[AnyRef]) &&
          (walue4.asInstanceOf[AnyRef] eq value4.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map4(key1, walue1, key2, walue2, key3, walue3, key4, walue4)
    }
    private[immutable] def buildTo[V1 >: V](builder: HashMapBuilder[K, V1]): builder.type =
      builder.addOne(key1, value1).addOne(key2, value2).addOne(key3, value3).addOne(key4, value4)
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 4
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key3, value3)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key4, value4)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]

private[immutable] final class MapBuilderImpl[K, V] extends ReusableBuilder[(K, V), Map[K, V]] {
  private[this] var elems: Map[K, V] = Map.empty
  private[this] var switchedToHashMapBuilder: Boolean = false
  private[this] var hashMapBuilder: HashMapBuilder[K, V] = _

  private[immutable] def getOrElse[V0 >: V](key: K, value: V0): V0 =
    if (hashMapBuilder ne null) hashMapBuilder.getOrElse(key, value)
    else elems.getOrElse(key, value)

  override def clear(): Unit = {
    elems = Map.empty
    if (hashMapBuilder != null) {
      hashMapBuilder.clear()
    }
    switchedToHashMapBuilder = false
  }

  override def result(): Map[K, V] =
    if (switchedToHashMapBuilder) hashMapBuilder.result() else elems

  def addOne(key: K, value: V): this.type = {
    if (switchedToHashMapBuilder) {
      hashMapBuilder.addOne(key, value)
    } else if (elems.size < 4) {
      elems = elems.updated(key, value)
    } else {
      // assert(elems.size == 4)
      if (elems.contains(key)) {
        elems = elems.updated(key, value)
      } else {
        switchedToHashMapBuilder = true
        if (hashMapBuilder == null) {
          hashMapBuilder = new HashMapBuilder
        }
        elems.asInstanceOf[Map4[K, V]].buildTo(hashMapBuilder)
        hashMapBuilder.addOne(key, value)
      }
    }

    this
  }

  def addOne(elem: (K, V)) = addOne(elem._1, elem._2)

  override def addAll(xs: IterableOnce[(K, V)]): this.type =
    if (switchedToHashMapBuilder) {
      hashMapBuilder.addAll(xs)
      this
    } else {
      super.addAll(xs)
    }
}
