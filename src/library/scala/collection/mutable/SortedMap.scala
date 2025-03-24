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
package collection.mutable

import scala.collection.{SortedMapFactory, SortedMapFactoryDefaults}

/**
  * Base type for mutable sorted map collections
  */
trait SortedMap[K, V]
  extends collection.SortedMap[K, V]
    with Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedMap, Iterable, Map] {

  override def unsorted: Map[K, V] = this

  override def sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap

  /** The same sorted map with a given default function.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefault`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     the function mapping keys to values, used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  override def withDefault(d: K => V): SortedMap[K, V] = new SortedMap.WithDefault[K, V](this, d)

  /** The same map with a given default value.
    * Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    * are not affected by `withDefaultValue`.
    *
    * Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    * @param d default value used for non-present keys
    * @return a wrapper of the map with a default value
    */
  override def withDefaultValue(d: V): SortedMap[K, V] = new SortedMap.WithDefault[K, V](this, _ => d)
}

trait SortedMapOps[K, V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends collection.SortedMapOps[K, V, CC, C]
    with MapOps[K, V, Map, C] {

  def unsorted: Map[K, V]

  @deprecated("Use m.clone().addOne((k,v)) instead of m.updated(k, v)", "2.13.0")
  override def updated[V1 >: V](key: K, value: V1): CC[K, V1] =
    clone().asInstanceOf[CC[K, V1]].addOne((key, value))
}

@SerialVersionUID(3L)
object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap) {

  @SerialVersionUID(3L)
  final class WithDefault[K, V](underlying: SortedMap[K, V], defaultValue: K => V)
    extends Map.WithDefault[K, V](underlying, defaultValue)
      with SortedMap[K, V]
      with SortedMapOps[K, V, SortedMap, WithDefault[K, V]]
      with Serializable {

    override def sortedMapFactory: SortedMapFactory[SortedMap] = underlying.sortedMapFactory

    def iteratorFrom(start: K): scala.collection.Iterator[(K, V)] = underlying.iteratorFrom(start)

    def keysIteratorFrom(start: K): scala.collection.Iterator[K] = underlying.keysIteratorFrom(start)

    implicit def ordering: Ordering[K] = underlying.ordering

    def rangeImpl(from: Option[K], until: Option[K]): WithDefault[K, V] =
      new WithDefault[K, V](underlying.rangeImpl(from, until), defaultValue)

    // Need to override following methods to match type signatures of `SortedMap.WithDefault`
    // for operations preserving default value
    override def subtractOne(elem: K): WithDefault.this.type = { underlying.subtractOne(elem); this }

    override def addOne(elem: (K, V)): WithDefault.this.type = { underlying.addOne(elem); this }

    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    override def concat[V2 >: V](suffix: collection.IterableOnce[(K, V2)]): SortedMap[K, V2] =
      underlying.concat(suffix).withDefault(defaultValue)

    override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)]): WithDefault[K, V] =
      new WithDefault[K, V](sortedMapFactory.from(coll), defaultValue)

    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] =
      SortedMap.newBuilder.mapResult((p: SortedMap[K, V]) => new WithDefault[K, V](p, defaultValue))
  }
}
