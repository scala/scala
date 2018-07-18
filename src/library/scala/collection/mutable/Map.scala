package scala
package collection
package mutable

import scala.language.higherKinds

/** Base type of mutable Maps */
trait Map[K, V]
  extends Iterable[(K, V)]
    with collection.Map[K, V]
    with MapOps[K, V, Map, Map[K, V]]
    with Growable[(K, V)]
    with Shrinkable[K] {

  override def mapFactory: scala.collection.MapFactory[MapCC] = Map

  /*
  //TODO consider keeping `remove` because it returns the removed entry
  @deprecated("Use subtract or -= instead of remove", "2.13.0")
  def remove(key: K): Option[V] = {
    val old = get(key)
    if(old.isDefined) subtract(key)
    old
  }
  */

  /** The same map with a given default function.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     the function mapping keys to values, used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefault(d: K => V): Map.WithDefault[K, V] = new Map.WithDefault[K, V](this, d)

  /** The same map with a given default value.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     default value used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefaultValue(d: V): Map.WithDefault[K, V] = new Map.WithDefault[K, V](this, x => d)

  @deprecated("Use - or remove on an immutable Map", "2.13.0")
  def - (key: K): Map[K, V] = clone() -= key
  @deprecated("Use -- or removeAll on an immutable Map", "2.13.0")
  def - (key1: K, key2: K, keys: K*): Map[K, V] = clone() -= key1 -= key2 --= keys
  @deprecated("Consider requiring an immutable Map.", "2.13.0")
  def -- (keys: IterableOnce[K]): Map[K, V] = {
    lazy val keysSet = keys.iterator.toSet
    fromSpecificIterable(this.filterKeys(k => !keysSet.contains(k)))
  }
  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  def + [V1 >: V](kv: (K, V1)): Map[K, V1] = mapFactory.from(new View.Appended(this, kv))
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): Map[K, V1] = this + elem1 + elem2 ++ elems

}

/**
  * @define coll mutable map
  * @define Coll `mutable.Map`
  */
trait MapOps[K, V, +CC[X, Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with Cloneable[C]
    with Growable[(K, V)]
    with Shrinkable[K] {

  /** Adds a new key/value pair to this map and optionally returns previously bound value.
    *  If the map already contains a
    *  mapping for the key, it will be overridden by the new value.
    *
    * @param key    the key to update
    * @param value  the new value
    * @return an option value containing the value associated with the key
    *         before the `put` operation was executed, or `None` if `key`
    *         was not defined in the map before.
    */
  def put(key: K, value: V): Option[V] = {
    val r = get(key)
    update(key, value)
    r
  }

  /** Adds a new key/value pair to this map.
    *  If the map already contains a
    *  mapping for the key, it will be overridden by the new value.
    *
    *  @param key    The key to update
    *  @param value  The new value
    */
  def update(key: K, value: V): Unit = { coll += ((key, value)) }

  /** If given key is already in this map, returns associated value.
   *
   *  Otherwise, computes value from given expression `op`, stores with key
   *  in map and returns that value.
   *
   *  Concurrent map implementations may evaluate the expression `op`
   *  multiple times, or may evaluate `op` without inserting the result.
   *
   *  @param  key the key to test
   *  @param  op  the computation yielding the value to associate with `key`, if
   *              `key` is previously unbound.
   *  @return     the value associated with key (either previously or as a result
   *              of executing the method).
   */
  def getOrElseUpdate(key: K, op: => V): V =
    get(key) match {
      case Some(v) => v
      case None => val d = op; this(key) = d; d
    }

  /** Removes a key from this map, returning the value associated previously
    *  with that key as an option.
    *  @param    key the key to be removed
    *  @return   an option value containing the value associated previously with `key`,
    *            or `None` if `key` was not defined in the map before.
    */
  def remove(key: K): Option[V] = {
    val r = get(key)
    if (r.isDefined) this -= key
    r
  }

  def clear(): Unit = { keysIterator foreach -= }

  override def clone(): C = empty ++= toIterable

  @deprecated("Use filterInPlace instead", "2.13.0")
  @inline final def retain(p: (K, V) => Boolean): this.type = filterInPlace(p)

  /** Retains only those mappings for which the predicate
    *  `p` returns `true`.
    *
    * @param p  The test predicate
    */
  def filterInPlace(p: (K, V) => Boolean): this.type = {
    for ((k, v) <- this.toList) // scala/bug#7269 toList avoids ConcurrentModificationException
      if (!p(k, v)) this -= k

    this
  }

  @deprecated("Use mapValuesInPlace instead", "2.13.0")
  @inline final def transform(f: (K, V) => V): this.type = mapValuesInPlace(f)

  /** Applies a transformation function to all values contained in this map.
    * The transformation function produces new values from existing keys
    * associated values.
    *
    * @param f  the transformation to apply
    * @return   the map itself.
    */
  def mapValuesInPlace(f: (K, V) => V): this.type = {
    iterator foreach {
      case (key, value) => update(key, f(key, value))
    }
    this
  }

}

/**
  * $factoryInfo
  * @define coll mutable map
  * @define Coll `mutable.Map`
  */
@SerialVersionUID(3L)
object Map extends MapFactory.Delegate[Map](HashMap) {

  class WithDefault[K, V](val underlying: Map[K, V], val defaultValue: K => V)
    extends AbstractMap[K, V]
      with MapOps[K, V, Map, WithDefault[K, V]] {

    override def default(key: K): V = defaultValue(key)

    def iterator: scala.collection.Iterator[(K, V)] = underlying.iterator

    override def mapFactory: MapFactory[Map] = underlying.mapFactory

    override def clear(): Unit = underlying.clear()

    def get(key: K): Option[V] = underlying.get(key)

    def -= (elem: K): WithDefault.this.type = { underlying.subtractOne(elem); this }

    def += (elem: (K, V)): WithDefault.this.type = { underlying.addOne(elem); this }

    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    override protected def fromSpecificIterable(coll: scala.collection.Iterable[(K, V)]): WithDefault[K, V] =
      new WithDefault[K, V](mapFactory.from(coll), defaultValue)

    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] =
      Map.newBuilder.mapResult((p: Map[K, V]) => new WithDefault[K, V](p, defaultValue))
  }

}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractMap[K, V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
