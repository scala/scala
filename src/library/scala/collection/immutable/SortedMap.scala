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
package immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder

/** An immutable map whose key-value pairs are sorted according to an [[scala.math.Ordering]] on the keys.
  *
  * Allows for range queries to be performed on its keys, and implementations must guarantee that traversal happens in
  * sorted order, according to the map's [[scala.math.Ordering]].
  *
  *  @example {{{
  *  import scala.collection.immutable.SortedMap
  *
  *  // Make a SortedMap via the companion object factory
  *  val weekdays = SortedMap(
  *    2 -> "Monday",
  *    3 -> "Tuesday",
  *    4 -> "Wednesday",
  *    5 -> "Thursday",
  *    6 -> "Friday"
  *  )
  *  // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday)
  *
  *  val days = weekdays ++ List(1 -> "Sunday", 7 -> "Saturday")
  *  // TreeMap(1 -> Sunday, 2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday, 7 -> Saturday)
  *
  *  val day3 = days.get(3) // Some("Tuesday")
  *
  *  val rangeOfDays = days.range(2, 5) // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday)
  *
  *  val daysUntil2 = days.rangeUntil(2) // TreeMap(1 -> Sunday)
  *  val daysTo2 = days.rangeTo(2) // TreeMap(1 -> Sunday, 2 -> Monday)
  *  val daysAfter5 = days.rangeFrom(5) //  TreeMap(5 -> Thursday, 6 -> Friday, 7 -> Saturday)
  *  }}}
  *
  *  @tparam K the type of the keys contained in this tree map.
  *  @tparam V the type of the values associated with the keys.
  */
trait SortedMap[K, +V]
  extends Map[K, V]
    with collection.SortedMap[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedMap, Iterable, Map] {

  override def unsorted: Map[K, V] = this

  override def sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap

  /** The same map with a given default function.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefault`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     the function mapping keys to values, used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  override def withDefault[V1 >: V](d: K => V1): SortedMap[K, V1] = new SortedMap.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
    *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
    *  are not affected by `withDefaultValue`.
    *
    *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
    *
    *  @param d     default value used for non-present keys
    *  @return      a wrapper of the map with a default value
    */
  override def withDefaultValue[V1 >: V](d: V1): SortedMap[K, V1] = new SortedMap.WithDefault[K, V1](this, _ => d)
}

trait SortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C] with collection.SortedMapOps[K, V, CC, C] { self =>

  protected def coll: C with CC[K, V]

  def unsorted: Map[K, V]

  override def keySet: SortedSet[K] = new ImmutableKeySortedSet

  /** The implementation class of the set returned by `keySet` */
  protected class ImmutableKeySortedSet extends AbstractSet[K] with SortedSet[K] with GenKeySet with GenKeySortedSet {
    def rangeImpl(from: Option[K], until: Option[K]): SortedSet[K] = {
      val map = self.rangeImpl(from, until)
      new map.ImmutableKeySortedSet
    }
    def incl(elem: K): SortedSet[K] = fromSpecific(this).incl(elem)
    def excl(elem: K): SortedSet[K] = fromSpecific(this).excl(elem)
  }

  // We override these methods to fix their return type (which would be `Map` otherwise)
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]
  @`inline` final override def +[V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)
  override def updatedWith[V1 >: V](key: K)(remappingFunction: Option[V] => Option[V1]): CC[K, V1] = {
    // Implementation has been copied from `MapOps`
    val previousValue = this.get(key)
    remappingFunction(previousValue) match {
      case None            => previousValue.fold(coll)(_ => this.removed(key).coll)
      case Some(nextValue) =>
        if (previousValue.exists(_.asInstanceOf[AnyRef] eq nextValue.asInstanceOf[AnyRef])) coll
        else coll.updated(key, nextValue)
    }
  }
  override def transform[W](f: (K, V) => W): CC[K, W] = map({ case (k, v) => (k, f(k, v)) })(ordering)
}

trait StrictOptimizedSortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends SortedMapOps[K, V, CC, C]
    with collection.StrictOptimizedSortedMapOps[K, V, CC, C]
    with StrictOptimizedMapOps[K, V, Map, C] {

  override def concat[V2 >: V](xs: collection.IterableOnce[(K, V2)]): CC[K, V2] = {
    var result: CC[K, V2] = coll
    val it = xs.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}

@SerialVersionUID(3L)
object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap) {

  override def from[K: Ordering, V](it: IterableOnce[(K, V)]): SortedMap[K, V] = it match {
    case sm: SortedMap[K, V] if Ordering[K] == sm.ordering => sm
    case _ => super.from(it)
  }

  final class WithDefault[K, +V](underlying: SortedMap[K, V], defaultValue: K => V)
    extends Map.WithDefault[K, V](underlying, defaultValue)
      with SortedMap[K, V]
      with SortedMapOps[K, V, SortedMap, WithDefault[K, V]] with Serializable {

    implicit def ordering: Ordering[K] = underlying.ordering

    override def sortedMapFactory: SortedMapFactory[SortedMap] = underlying.sortedMapFactory

    def iteratorFrom(start: K): scala.collection.Iterator[(K, V)] = underlying.iteratorFrom(start)

    def keysIteratorFrom(start: K): scala.collection.Iterator[K] = underlying.keysIteratorFrom(start)

    def rangeImpl(from: Option[K], until: Option[K]): WithDefault[K, V] =
      new WithDefault[K, V](underlying.rangeImpl(from, until), defaultValue)

    // Need to override following methods to match type signatures of `SortedMap.WithDefault`
    // for operations preserving default value

    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] =
      new WithDefault[K, V1](underlying.updated(key, value), defaultValue)

    override def concat [V2 >: V](xs: collection.IterableOnce[(K, V2)]): WithDefault[K, V2] =
      new WithDefault( underlying.concat(xs) , defaultValue)

    override def removed(key: K): WithDefault[K, V] = new WithDefault[K, V](underlying.removed(key), defaultValue)

    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)] @uncheckedVariance): WithDefault[K, V] =
      new WithDefault[K, V](sortedMapFactory.from(coll), defaultValue)

    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] @uncheckedVariance =
      SortedMap.newBuilder.mapResult((p: SortedMap[K, V]) => new WithDefault[K, V](p, defaultValue))
  }
}
