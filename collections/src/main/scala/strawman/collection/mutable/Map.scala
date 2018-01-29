package strawman
package collection
package mutable

import strawman.collection.{IterableOnce, MapFactory}

import scala.{Boolean, None, Option, Some, Unit, `inline`, deprecated}

/** Base type of mutable Maps */
trait Map[K, V]
  extends Iterable[(K, V)]
    with collection.Map[K, V]
    with MapOps[K, V, Map, Map[K, V]] {

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
    with Growable[(K, V)]
    with Shrinkable[K] {

  def iterableFactory: IterableFactory[Iterable] = Iterable

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

  /** Retains only those mappings for which the predicate
    *  `p` returns `true`.
    *
    * @param p  The test predicate
    */
  @deprecated("Use .filterInPlace instead of .retain", "2.13.0")
  @`inline` final def retain(p: (K, V) => Boolean): this.type = filterInPlace(p.tupled)
}

/**
  * $factoryInfo
  * @define coll mutable map
  * @define Coll `mutable.Map`
  */
object Map extends MapFactory.Delegate[Map](HashMap) {
  final class WithDefault[K, V](underlying: Map[K, V], d: K => V) extends Map[K, V] {
    // These factory methods will lose the default value
    override def iterableFactory = underlying.iterableFactory
    def mapFactory: MapFactory[Map] = underlying.mapFactory
    protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2,V2] = mapFactory.from(it)

    // Specific building will keep the default but may lose the precise underlying type because our own V can be
    // a supertype of the underlying collection's V so we cannot rebuild with potentially new values that are not
    // valid for the underlying collection.
    protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): Map[K,V] = new WithDefault[K, V](mapFactory.from(coll), d)
    protected[this] def newSpecificBuilder(): mutable.Builder[(K, V), Map[K,V]] =
      mapFactory.newBuilder[K, V]().mapResult(new WithDefault[K, V](_, d))
    override def size = underlying.size
    def get(key: K) = underlying.get(key)
    def iterator() = underlying.iterator()
    override def default(key: K): V = d(key)
    override def empty = new WithDefault(underlying.empty, d)

    def clear(): Unit = underlying.clear()
    def addOne(elem: (K, V)): this.type = { underlying.addOne(elem); this }
    def subtractOne(elem: K): this.type = { underlying.subtractOne(elem); this }
    override def put(key: K, value: V): Option[V] = underlying.put(key, value)
    override def update(key: K, value: V): Unit = underlying.update(key, value)
    override def remove(key: K): Option[V] = underlying.remove(key)

    override def withDefault(d: K => V): Map[K, V] = new WithDefault[K, V](underlying, d)
    override def withDefaultValue(d: V): Map[K, V] = new WithDefault[K, V](underlying, x => d)
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[A, B] extends strawman.collection.AbstractMap[A, B] with Map[A, B]
