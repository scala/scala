/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package immutable

import generic._

/**
 * A generic trait for immutable maps. Concrete classes have to provide
 * functionality for the abstract methods in `Map`:
 *
 * {{{
 *    def get(key: A): Option[B]
 *    def iterator: Iterator[(A, B)]
 *    def + [B1 >: B](kv: (A, B1)): Map[A, B1]
 *    def -(key: A): Map[A, B]
 * }}}
 *
 * @since 1
 */
trait Map[A, +B] extends Iterable[(A, B)]
//                    with GenMap[A, B]
                    with scala.collection.Map[A, B]
                    with MapLike[A, B, Map[A, B]] { self =>

  override def empty: Map[A, B] = Map.empty

  /** Returns this $coll as an immutable map.
   *
   *  A new map will not be built; lazy collections will stay lazy.
   */
  @deprecatedOverriding("Immutable maps should do nothing on toMap except return themselves cast as a map.",  "2.11.0")
  override def toMap[T, U](implicit ev: (A, B) <:< (T, U)): immutable.Map[T, U] =
    self.asInstanceOf[immutable.Map[T, U]]

  override def seq: Map[A, B] = this

  /** The same map with a given default function.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault[B1 >: B](d: A => B1): immutable.Map[A, B1] = new Map.WithDefault[A, B1](this, d)

  /** The same map with a given default value.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue[B1 >: B](d: B1): immutable.Map[A, B1] = new Map.WithDefault[A, B1](this, x => d)

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated [B1 >: B](key: A, value: B1): Map[A, B1]
  def + [B1 >: B](kv: (A, B1)): Map[A, B1]
}

/** $factoryInfo
 *  @define Coll `immutable.Map`
 *  @define coll immutable map
 */
object Map extends ImmutableMapFactory[Map] {

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Map[A, B]] = new MapCanBuildFrom[A, B]

  def empty[A, B]: Map[A, B] = EmptyMap.asInstanceOf[Map[A, B]]

  class WithDefault[A, +B](underlying: Map[A, B], d: A => B) extends scala.collection.Map.WithDefault[A, B](underlying, d) with Map[A, B] {
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[B1 >: B](key: A, value: B1): WithDefault[A, B1] = new WithDefault[A, B1](underlying.updated[B1](key, value), d)
    override def + [B1 >: B](kv: (A, B1)): WithDefault[A, B1] = updated(kv._1, kv._2)
    override def - (key: A): WithDefault[A, B] = new WithDefault(underlying - key, d)
    override def withDefault[B1 >: B](d: A => B1): immutable.Map[A, B1] = new WithDefault[A, B1](underlying, d)
    override def withDefaultValue[B1 >: B](d: B1): immutable.Map[A, B1] = new WithDefault[A, B1](underlying, x => d)
  }

  private object EmptyMap extends AbstractMap[Any, Nothing] with Map[Any, Nothing] with Serializable {
    override def size: Int = 0
    def get(key: Any): Option[Nothing] = None
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def updated [B1] (key: Any, value: B1): Map[Any, B1] = new Map1(key, value)
    def + [B1](kv: (Any, B1)): Map[Any, B1] = updated(kv._1, kv._2)
    def - (key: Any): Map[Any, Nothing] = this
  }

  class Map1[A, +B](key1: A, value1: B) extends AbstractMap[A, B] with Map[A, B] with Serializable {
    override def size = 1
    def get(key: A): Option[B] =
      if (key == key1) Some(value1) else None
    def iterator = Iterator((key1, value1))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((A, B)) => U): Unit = {
      f((key1, value1))
    }
  }

  class Map2[A, +B](key1: A, value1: B, key2: A, value2: B) extends AbstractMap[A, B] with Map[A, B] with Serializable {
    override def size = 2
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    def iterator = Iterator((key1, value1), (key2, value2))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((A, B)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
  }

  class Map3[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B) extends AbstractMap[A, B] with Map[A, B] with Serializable {
    override def size = 3
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((A, B)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
  }

  class Map4[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B, key4: A, value4: B) extends AbstractMap[A, B] with Map[A, B] with Serializable {
    override def size = 4
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else new HashMap + ((key1, value1), (key2, value2), (key3, value3), (key4, value4), (key, value))
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((A, B)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[A, +B] extends scala.collection.AbstractMap[A, B] with Map[A, B]
