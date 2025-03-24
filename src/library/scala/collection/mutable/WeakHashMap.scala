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

import scala.annotation.nowarn
import scala.collection.convert.JavaCollectionWrappers.{JMapWrapper, JMapWrapperLike}

/** A hash map with references to entries which are weakly reachable. Entries are
 *  removed from this map when the key is no longer (strongly) referenced. This class wraps
 *  `java.util.WeakHashMap`.
 *
 *  @tparam K      type of keys contained in this map
 *  @tparam V      type of values associated with the keys
 *
 *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#weak-hash-maps "Scala's Collection Library overview"]]
 *  section on `Weak Hash Maps` for more information.
 *
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3L)
class WeakHashMap[K, V] extends JMapWrapper[K, V](new java.util.WeakHashMap)
    with JMapWrapperLike[K, V, WeakHashMap, WeakHashMap[K, V]]
    with MapFactoryDefaults[K, V, WeakHashMap, Iterable] {
  override def empty = new WeakHashMap[K, V]
  override def mapFactory: MapFactory[WeakHashMap] = WeakHashMap
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
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
  def newBuilder[K, V]: Builder[(K, V), WeakHashMap[K,V]] = new GrowableBuilder(WeakHashMap.empty[K, V])
}

