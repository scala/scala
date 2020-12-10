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

  override def newBuilder[A, B]: mutable.Builder[(A, B), Map[A, B]] = new MapBuilderImpl[A, B]

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (K, V), Map[K, V]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Nothing, Nothing]

  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  @SerialVersionUID(-7464981207502461188L)
  class WithDefault[K, +V](underlying: Map[K, V], d: K => V) extends scala.collection.Map.WithDefault[K, V](underlying, d) with Map[K, V] {
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] = new WithDefault[K, V1](underlying.updated[V1](key, value), d)
    override def + [V1 >: V](kv: (K, V1)): WithDefault[K, V1] = updated(kv._1, kv._2)
    override def - (key: K): WithDefault[K, V] = new WithDefault(underlying - key, d)
    override def withDefault[V1 >: V](d: K => V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, d)
    override def withDefaultValue[V1 >: V](d: V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, x => d)
  }

  @SerialVersionUID(-5626373049574850357L)
  private object EmptyMap extends AbstractMap[Any, Nothing] with Map[Any, Nothing] with Serializable with HasForeachEntry[Any, Nothing]{
    override def size: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    override def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def keysIterator: Iterator[Any] = Iterator.empty
    override def valuesIterator: Iterator[Nothing] = Iterator.empty
    override def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    def + [V1](kv: (Any, V1)): Map[Any, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: Nothing](xs: GenTraversableOnce[(Any, V1)]): Map[Any, V1] = ++[(Any, V1), Map[Any, V1]](xs)(Map.canBuildFrom[Any, V1])
    override def ++[B >: (Any, Nothing), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Map[Any, Nothing], B, That]): That = {
      if (isMapCBF(bf))
        that match {
          case hm: HashMap[a, b] if hm.size > 4 => hm.asInstanceOf[That]
          case m: AnyRef if m eq EmptyMap => this.asInstanceOf[That]
          case m: Map1[_, _] => m.asInstanceOf[That]
          case m: Map2[_, _] => m.asInstanceOf[That]
          case m: Map3[_, _] => m.asInstanceOf[That]
          case m: Map4[_, _] => m.asInstanceOf[That]

          case _ => super.++(that)(bf)
        }
      else if (isHashMapCBF(bf))
        that match {
          case hm: HashMap[a, b] => hm.asInstanceOf[That]

          case _ => super.++(that)(bf)
        }
      else super.++(that)(bf)
    }
    def - (key: Any): Map[Any, Nothing] = this
    override def hashCode: Int = MurmurHash3.emptyMapHash
    override private[immutable] def foreachEntry[U](f: (Any, Nothing) => U): Unit = ()
  }
  @SerialVersionUID(3L)
  private abstract class MapNIterator[T]() extends AbstractIterator[T] with Serializable {
    private[this] var current = 0
    def hasNext = current < size
    def apply(i: Int): T
    def size: Int
    def next(): T =
      if (hasNext) {
        val r = apply(current)
        current += 1
        r
      } else Iterator.empty.next()

    override def drop(n: Int): Iterator[T] = {
      if (n > 0) current = Math.min(current + n, size)
      this
    }
  }

  @SerialVersionUID(-9131943191104946031L)
  class Map1[K, +V](key1: K, value1: V) extends AbstractMap[K, V] with Map[K, V] with Serializable with HasForeachEntry[K, V] {
    override def size = 1
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    override def iterator = Iterator.single((key1, value1))
    override def keysIterator: Iterator[K] = Iterator.single(key1)
    override def valuesIterator: Iterator[V] = Iterator.single(value1)
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] = ++[(K, V1), Map[K, V1]](xs)(Map.canBuildFrom[K, V1])
    override def ++[B >: (K, V), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Map[K, V], B, That]): That = {
      if (isMapCBF(bf)) that match {
        case m: AnyRef if m eq EmptyMap => this.asInstanceOf[That]
        case m: Map1[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map2[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map3[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map4[K, V] => m.addTo(this).asInstanceOf[That]
        case _             => super.++(that)(bf)
      } else super.++(that)(bf)
    }
    private[Map] def addTo[V1 >: V](m : Map[K,V1]): Map[K, V1] = {
      m.updated(key1, value1)
    }
    def - (key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    override private[scala] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] =
      if (pred((key1, value1)) != isFlipped) this else Map.empty
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
    override private[immutable] def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
    }
  }

  @SerialVersionUID(-85684685400398742L)
  class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends AbstractMap[K, V] with Map[K, V] with Serializable with HasForeachEntry[K, V] {
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
    //we have to insert these additional methods to avoid the compiler rewriting the field names and changing binary format
    private def _getKey(i: Int) = i match { case 0 => key1 case 1 => key2}
    private def _getValue(i: Int) = i match { case 0 => value1 case 1 => value2}
    private abstract class Map2Iterator[T] extends MapNIterator[T]{
      final def getKey(i: Int) = _getKey(i)
      final def getValue(i: Int) = _getValue(i)
      override final def size = 2
    }
    override def iterator: Iterator[(K,V)] = new Map2Iterator[(K,V)] {def apply(i: Int) = (getKey(i), getValue(i))}
    override def keysIterator: Iterator[K] = new Map2Iterator[K] {def apply(i: Int) = getKey(i)}
    override def valuesIterator: Iterator[V] = new Map2Iterator[V] {def apply(i: Int) = getValue(i)}
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] = ++[(K, V1), Map[K, V1]](xs)(Map.canBuildFrom[K, V1])
    override def ++[B >: (K, V), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Map[K, V], B, That]): That = {
      if (isMapCBF(bf)) that match {
        case m: AnyRef if m eq EmptyMap => this.asInstanceOf[That]
        case m: Map1[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map2[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map3[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map4[K, V] => m.addTo(this).asInstanceOf[That]
        case _             => super.++(that)(bf)
      } else super.++(that)(bf)
    }
    private[Map] def addTo[V1 >: V](m : Map[K,V1]): Map[K, V1] = {
      m.updated(key1, value1).
       updated(key2, value2)
    }
    def - (key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2))
    override private[scala] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
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
    override private[immutable] def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
    }
  }

  @SerialVersionUID(-6400718707310517135L)
  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends AbstractMap[K, V] with Map[K, V] with Serializable with HasForeachEntry[K, V] {
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
    //we have to insert these additional methods to avoid the compiler rewriting the field names and changing binary format
    private def _getKey(i: Int) = i match { case 0 => key1 case 1 => key2 case 2 => key3}
    private def _getValue(i: Int) = i match { case 0 => value1 case 1 => value2  case 2 => value3}
    private abstract class Map3Iterator[T] extends MapNIterator[T]{
      final def getKey(i: Int) = _getKey(i)
      final def getValue(i: Int) = _getValue(i)
      override final def size = 3
    }
    override def iterator: Iterator[(K,V)] = new Map3Iterator[(K,V)] {def apply(i: Int) = (getKey(i), getValue(i))}
    override def keysIterator: Iterator[K] = new Map3Iterator[K] {def apply(i: Int) = getKey(i)}
    override def valuesIterator: Iterator[V] = new Map3Iterator[V] {def apply(i: Int) = getValue(i)}
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] = ++[(K, V1), Map[K, V1]](xs)(Map.canBuildFrom[K, V1])
    override def ++[B >: (K, V), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Map[K, V], B, That]): That = {
      if (isMapCBF(bf)) that match {
        case m: AnyRef if m eq EmptyMap => this.asInstanceOf[That]
        case m: Map1[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map2[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map3[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map4[K, V] => m.addTo(this).asInstanceOf[That]
        case _             => super.++(that)(bf)
      } else super.++(that)(bf)
    }
    private[Map] def addTo[V1 >: V](m : Map[K,V1]): Map[K, V1] = {
      m.updated(key1, value1).
       updated(key2, value2).
       updated(key3, value3)
    }
    def - (key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2)) || p((key3, value3))
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2)) && p((key3, value3))
    override private[scala] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
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
    override private[immutable] def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
    }
  }

  @SerialVersionUID(-7992135791595275193L)
  class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends AbstractMap[K, V] with Map[K, V] with Serializable with HasForeachEntry[K, V] {
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
    //we have to insert these additional methods to avoid the compiler rewriting the field names and changing binary format
    private def _getKey(i: Int) = i match { case 0 => key1 case 1 => key2 case 2 => key3 case 3 => key4 }
    private def _getValue(i: Int) = i match { case 0 => value1 case 1 => value2  case 2 => value3 case 3 => value4}
    private abstract class Map4Iterator[T] extends MapNIterator[T]{
      final def getKey(i: Int) = _getKey(i)
      final def getValue(i: Int) = _getValue(i)
      override final def size = 4
    }
    override def iterator: Iterator[(K,V)] = new Map4Iterator[(K,V)] {def apply(i: Int) = (getKey(i), getValue(i))}
    override def keysIterator: Iterator[K] = new Map4Iterator[K] {def apply(i: Int) = getKey(i)}
    override def valuesIterator: Iterator[V] = new Map4Iterator[V] {def apply(i: Int) = getValue(i)}
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else (new HashMap).updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] = ++[(K, V1), Map[K, V1]](xs)(Map.canBuildFrom[K, V1])
    override def ++[B >: (K, V), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Map[K, V], B, That]): That = {
      if (isMapCBF(bf)) that match {
        case m: AnyRef if m eq EmptyMap => this.asInstanceOf[That]
        case m: Map1[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map2[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map3[K, V] => m.addTo(this).asInstanceOf[That]
        case m: Map4[K, V] => m.addTo(this).asInstanceOf[That]
        case _             => super.++(that)(bf)
      } else super.++(that)(bf)
    }
    private[Map] def addTo[V1 >: V](m : Map[K,V1]): Map[K, V1] = {
      m.updated(key1, value1).
       updated(key2, value2).
       updated(key3, value3).
       updated(key4, value4)
    }
    def - (key: K): Map[K, V] =
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
    override private[scala] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
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
    override private[immutable] def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
      f(key4, value4)
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

  private def isHashMapCBF(cbf: CanBuildFrom[_,_,_]) = {
    cbf match {
      case w: WrappedCanBuildFrom[_,_,_] =>
        val unwrapped = w.wrapped
        unwrapped eq HashMap.canBuildFrom
      case _ =>
        cbf eq HashMap.canBuildFrom
    }
  }
  private def isMapCBF(cbf: CanBuildFrom[_,_,_]) = {
    cbf match {
      case w: WrappedCanBuildFrom[_, _, _] =>
        val unwrapped = w.wrapped
        unwrapped eq Map.canBuildFrom
      case _ =>
        cbf eq Map.canBuildFrom
    }
  }

  /** Builder for Map.
   */
  private final class MapBuilderImpl[K, V] extends mutable.ReusableBuilder[(K,V), Map[K, V]] {

    private[this] var elems: Map[K, V] = Map.empty[K, V]
    private[this] var switchedToHashMapBuilder: Boolean = false
    private[this] var hashMapBuilder: HashMap.HashMapBuilder[K, V] = _

    override def clear(): Unit = {
      elems =Map.empty[K, V]
      if (hashMapBuilder ne null)
        hashMapBuilder.clear()
      switchedToHashMapBuilder = false
    }

    override def result(): Map[K, V] =
      if (switchedToHashMapBuilder) hashMapBuilder.result() else elems

    private def convertToHashMapBuilder(): Unit = {
      switchedToHashMapBuilder = true
      if (hashMapBuilder eq null)
        hashMapBuilder = new HashMap.HashMapBuilder[K, V]

      hashMapBuilder ++= elems
    }

    override def +=(elem: (K, V)): MapBuilderImpl.this.type = {
      if (switchedToHashMapBuilder) {
        hashMapBuilder += elem
      } else if (elems.size < 4) {
        elems = elems + elem
      } else {
        val key = elem._1
        val newValue = elem._2
        elems.getOrElse(key, Sentinel) match {
          case Sentinel =>
            convertToHashMapBuilder()
            hashMapBuilder += elem
          case existingValue =>
            if (existingValue.asInstanceOf[AnyRef] ne newValue.asInstanceOf[AnyRef])
              elems = elems + elem
        }
      }
      this
    }

    override def ++=(xs: TraversableOnce[(K, V)]): MapBuilderImpl.this.type = {
      xs match {
        case _ if switchedToHashMapBuilder =>
          hashMapBuilder ++= xs

        case map: collection.Map[K,V] if map.size > 4 =>
          convertToHashMapBuilder()
          hashMapBuilder ++= map

        case _ => super.++= (xs)
      }
      this
    }
  }
  private val Sentinel = new Object
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
