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

import generic._
import scala.util.hashing.MurmurHash3

/**
 * A generic trait for immutable maps. Concrete classes have to provide
 * functionality for the abstract methods in `Map`:
 *
 * {{{
 *    def get(key: K): Option[V]
 *    def iterator: Iterator[(K, V)]
 *    def + [V1 >: V](kv: (K, V1)): Map[K, V1]
 *    def -(key: K): Map[K, V]
 * }}}
 *
 * @since 1
 */
trait Map[K, +V] extends Iterable[(K, V)]
//                    with GenMap[K, V]
                    with scala.collection.Map[K, V]
                    with MapLike[K, V, Map[K, V]] { self =>

  override def empty: Map[K, V] = Map.empty

  /** Returns this $coll as an immutable map.
   *
   *  A new map will not be built; lazy collections will stay lazy.
   */
  @deprecatedOverriding("Immutable maps should do nothing on toMap except return themselves cast as a map.",  "2.11.0")
  override def toMap[T, U](implicit ev: (K, V) <:< (T, U)): immutable.Map[T, U] =
    self.asInstanceOf[immutable.Map[T, U]]

  override def seq: Map[K, V] = this

  /** The same map with a given default function.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault[V1 >: V](d: K => V1): immutable.Map[K, V1] = new Map.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue[V1 >: V](d: V1): immutable.Map[K, V1] = new Map.WithDefault[K, V1](this, x => d)

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated [V1 >: V](key: K, value: V1): Map[K, V1]
  def + [V1 >: V](kv: (K, V1)): Map[K, V1]
}

/** $factoryInfo
 *  @define Coll `immutable.Map`
 *  @define coll immutable map
 */
object Map extends ImmutableMapFactory[Map] {

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (K, V), Map[K, V]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Nothing, Nothing]

  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  class WithDefault[K, +V](underlying: Map[K, V], d: K => V) extends scala.collection.Map.WithDefault[K, V](underlying, d) with Map[K, V] {
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] = new WithDefault[K, V1](underlying.updated[V1](key, value), d)
    override def + [V1 >: V](kv: (K, V1)): WithDefault[K, V1] = updated(kv._1, kv._2)
    override def - (key: K): WithDefault[K, V] = new WithDefault(underlying - key, d)
    override def withDefault[V1 >: V](d: K => V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, d)
    override def withDefaultValue[V1 >: V](d: V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, x => d)
  }

  @SerialVersionUID(-5626373049574850357L)
  private object EmptyMap extends AbstractMap[Any, Nothing] with Map[Any, Nothing] with Serializable {
    override def size: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def + [V1](kv: (Any, V1)): Map[Any, V1] = updated(kv._1, kv._2)
    def - (key: Any): Map[Any, Nothing] = this
    override def hashCode: Int = MurmurHash3.emptyMapHash
  }

  @SerialVersionUID(-9131943191104946031L)
  class Map1[K, +V](key1: K, value1: V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
    override def size = 1
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    def iterator = Iterator((key1, value1))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 1
      var c = 1

      var h = MurmurHash3.product2Hash(key1, value1)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(-85684685400398742L)
  class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
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
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else default
    def iterator = Iterator((key1, value1), (key2, value2))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 2
      var c = 1

      var h = MurmurHash3.product2Hash(key1, value1)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key2, value2)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(-6400718707310517135L)
  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
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
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else default
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 3
      var c = 1

      var h = MurmurHash3.product2Hash(key1, value1)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key2, value2)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key3, value3)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  @SerialVersionUID(-7992135791595275193L)
  class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
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
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else default
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else (new HashMap).updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 4
      var c = 1

      var h = MurmurHash3.product2Hash(key1, value1)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key2, value2)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key3, value3)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.product2Hash(key4, value4)
      a += h
      b ^= h
      if (h != 0) c *= h

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }
  private [immutable] final class HashCodeAccumulator extends scala.runtime.AbstractFunction2[Any, Any, Unit] {
    import scala.util.hashing.MurmurHash3
    private var a, b, n = 0
    private var c = 1
    def apply(key: Any, value: Any): Unit = {
      val h = MurmurHash3.product2Hash(key, value)
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }

    def finalizeHash: Int = {
      var h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, n)
    }
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
