package strawman
package collection
package mutable

import strawman.collection.{IterableOnce, MapFactory}

import scala.{Boolean, None, Option, Some, Unit, `inline`}

/** Base type of mutable Maps */
trait Map[K, V]
  extends Iterable[(K, V)]
    with collection.Map[K, V]
    with MapOps[K, V, Map, Map[K, V]]

/** Base trait of mutable Maps implementations */
trait MapOps[K, V, +CC[X, Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with Shrinkable[K] {

  def iterableFactory = Iterable

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

  override def clone(): C = empty ++= toIterable

  def mapInPlace(f: ((K, V)) => (K, V)): this.type = {
    val toAdd = Map[K, V]()
    val toRemove = Set[K]()
    for (elem <- this) {
      val mapped = f(elem)
      if (!contains(mapped._1)) {
        toAdd += mapped
        toRemove -= elem._1
      }
    }
    for (elem <- toRemove) coll -= elem
    for (elem <- toAdd) coll += elem
    this
  }

  def flatMapInPlace(f: ((K, V)) => IterableOnce[(K, V)]): this.type = {
    val toAdd = Map[K, V]()
    val toRemove = Set[K]()
    for (elem <- this)
      for (mapped <- f(elem).iterator())
        if (!contains(mapped._1)) {
          toAdd += mapped
          toRemove -= elem._1
        }
    for (elem <- toRemove) coll -= elem
    for (elem <- toAdd) coll += elem
    this
  }

  def filterInPlace(p: ((K, V)) => Boolean): this.type = {
    val toRemove = Set[K]()
    for (elem <- this)
      if (!p(elem)) toRemove += elem._1
    for (elem <- toRemove)
      coll -= elem
    this
  }

}

object Map extends MapFactory.Delegate[Map](HashMap)
