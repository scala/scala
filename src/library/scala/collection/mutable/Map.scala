/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

/** Base type of mutable Maps */
trait Map[K, V]
  extends Iterable[(K, V)]
    with collection.Map[K, V]
    with MapOps[K, V, Map, Map[K, V]]
    with Growable[(K, V)]
    with Shrinkable[K]
    with MapFactoryDefaults[K, V, Map, Iterable] {

  override def mapFactory: scala.collection.MapFactory[Map] = Map

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
  def withDefault(d: K => V): Map[K, V] = new Map.WithDefault[K, V](this, d)

  /** The same map with a given default value.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     default value used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  def withDefaultValue(d: V): Map[K, V] = new Map.WithDefault[K, V](this, x => d)
}

/**
  * @define coll mutable map
  * @define Coll `mutable.Map`
  */
trait MapOps[K, V, +CC[X, Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with Cloneable[C]
    with Builder[(K, V), C]
    with Growable[(K, V)]
    with Shrinkable[K] {

  def result(): C = coll

  @deprecated("Use - or remove on an immutable Map", "2.13.0")
  final def - (key: K): C = clone() -= key

  @deprecated("Use -- or removeAll on an immutable Map", "2.13.0")
  final def - (key1: K, key2: K, keys: K*): C = clone() -= key1 -= key2 --= keys

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

  /**
   * Update a mapping for the specified key and its current optionally mapped value
   * (`Some` if there is current mapping, `None` if not).
   *
   * If the remapping function returns `Some(v)`, the mapping is updated with the new value `v`.
   * If the remapping function returns `None`, the mapping is removed (or remains absent if initially absent).
   * If the function itself throws an exception, the exception is rethrown, and the current mapping is left unchanged.
   *
   * @param key the key value
   * @param remappingFunction a function that receives current optionally mapped value and return a new mapping
   * @return the new value associated with the specified key
   */
  def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    val previousValue = this.get(key)
    val nextValue = remappingFunction(previousValue)
    (previousValue, nextValue) match {
      case (None, None) => // do nothing
      case (Some(_), None) => this.remove(key)
      case (_, Some(v)) => this.update(key,v)
    }
    nextValue
  }

  /** If given key is already in this map, returns associated value.
   *
   *  Otherwise, computes value from given expression `defaultValue`, stores with key
   *  in map and returns that value.
   *
   *  Concurrent map implementations may evaluate the expression `defaultValue`
   *  multiple times, or may evaluate `defaultValue` without inserting the result.
   *
   *  @param  key the key to test
   *  @param  defaultValue  the computation yielding the value to associate with `key`, if
   *              `key` is previously unbound.
   *  @return     the value associated with key (either previously or as a result
   *              of executing the method).
   */
  def getOrElseUpdate(key: K, @deprecatedName("op", since="2.13.13") defaultValue: => V): V =
    get(key) match {
      case Some(v) => v
      case None => val d = defaultValue; this(key) = d; d
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

  override def clone(): C = empty ++= this

  @deprecated("Use filterInPlace instead", "2.13.0")
  @inline final def retain(p: (K, V) => Boolean): this.type = filterInPlace(p)

  /** Retains only those mappings for which the predicate
    *  `p` returns `true`.
    *
    * @param p  The test predicate
    */
  def filterInPlace(p: (K, V) => Boolean): this.type = {
    if (!isEmpty) this match {
      case tm: concurrent.Map[_, _] => tm.asInstanceOf[concurrent.Map[K, V]].filterInPlaceImpl(p)
      case _ =>
        val array = this.toArray[Any] // scala/bug#7269 toArray avoids ConcurrentModificationException
        val arrayLength = array.length
        var i = 0
        while (i < arrayLength) {
          val (k, v) = array(i).asInstanceOf[(K, V)]
          if (!p(k, v)) {
            this -= k
          }
          i += 1
        }
    }
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
    if (!isEmpty) this match {
      case hm: mutable.HashMap[_, _] => hm.asInstanceOf[mutable.HashMap[K, V]].mapValuesInPlaceImpl(f)
      case tm: concurrent.Map[_, _] => tm.asInstanceOf[concurrent.Map[K, V]].mapValuesInPlaceImpl(f)
      case _ =>
        val array = this.toArray[Any]
        val arrayLength = array.length
        var i = 0
        while (i < arrayLength) {
          val (k, v) = array(i).asInstanceOf[(K, V)]
          update(k, f(k, v))
          i += 1
        }
    }
    this
  }

  @deprecated("Use m.clone().addOne((k,v)) instead of m.updated(k, v)", "2.13.0")
  def updated[V1 >: V](key: K, value: V1): CC[K, V1] =
    clone().asInstanceOf[CC[K, V1]].addOne((key, value))

  override def knownSize: Int = super[IterableOps].knownSize
}

/**
  * $factoryInfo
  * @define coll mutable map
  * @define Coll `mutable.Map`
  */
@SerialVersionUID(3L)
object Map extends MapFactory.Delegate[Map](HashMap) {

  @SerialVersionUID(3L)
  class WithDefault[K, V](val underlying: Map[K, V], val defaultValue: K => V)
    extends AbstractMap[K, V]
      with MapOps[K, V, Map, WithDefault[K, V]] with Serializable {

    override def default(key: K): V = defaultValue(key)

    def iterator: scala.collection.Iterator[(K, V)] = underlying.iterator
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = underlying.knownSize
    override def mapFactory: MapFactory[Map] = underlying.mapFactory

    override def clear(): Unit = underlying.clear()

    def get(key: K): Option[V] = underlying.get(key)

    def subtractOne(elem: K): WithDefault.this.type = { underlying.subtractOne(elem); this }

    def addOne(elem: (K, V)): WithDefault.this.type = { underlying.addOne(elem); this }

    override def concat[V2 >: V](suffix: collection.IterableOnce[(K, V2)]): Map[K, V2] =
      underlying.concat(suffix).withDefault(defaultValue)

    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)]): WithDefault[K, V] =
      new WithDefault[K, V](mapFactory.from(coll), defaultValue)

    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] =
      Map.newBuilder.mapResult((p: Map[K, V]) => new WithDefault[K, V](p, defaultValue))
  }

}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
