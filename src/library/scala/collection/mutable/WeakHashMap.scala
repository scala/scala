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
package mutable

import convert.Wrappers._

/** A hash map with references to entries which are weakly reachable. Entries are
 *  removed from this map when the key is no longer (strongly) referenced. This class wraps
 *  `java.util.WeakHashMap`.
 *
 *  @tparam K      type of keys contained in this map
 *  @tparam V      type of values associated with the keys
 *
 *  @since 2.8
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#weak-hash-maps "Scala's Collection Library overview"]]
 *  section on `Weak Hash Maps` for more information.
 *
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
class WeakHashMap[K, V] extends JMapWrapper[K, V](new java.util.WeakHashMap)
    with JMapWrapperLike[K, V, WeakHashMap, WeakHashMap[K, V]] {
  override def empty = new WeakHashMap[K, V]
  override def mapFactory: MapFactory[WeakHashMap] = WeakHashMap
  override protected[this] def stringPrefix = "WeakHashMap"
}

/** $factoryInfo
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 */
@SerialVersionUID(3L)
object WeakHashMap extends MapFactory[WeakHashMap] {
  def empty[K, V]: WeakHashMap[K,V] = new WeakHashMap[K, V]
  def from[K, V](it: collection.IterableOnce[(K, V)]): WeakHashMap[K,V] = Growable.from(empty[K, V], it)

  def groupFrom[A, K, C1](it: IterableOnce[A],
                          f: A => K,
                          builder: => mutable.Builder[A, C1]): WeakHashMap[K, C1] = {
    val iterator = it.iterator
    if (iterator.isEmpty) {
      empty
    } else {
      val m = mutable.Map.empty[K, Builder[A, C1]]
      val it = iterator
      while (it.hasNext) {
        val elem = it.next()
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, builder)
        bldr += elem
      }
      val result = empty[K, C1]
      val mapIt = m.iterator
      while (mapIt.hasNext) {
        val (k, v) = mapIt.next()
        result.update(k, v.result())
      }
      result
    }
  }

  def newBuilder[K, V]: Builder[(K, V), WeakHashMap[K,V]] = new GrowableBuilder(WeakHashMap.empty[K, V])
}

