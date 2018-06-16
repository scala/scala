/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import convert.Wrappers._

/** A hash map with references to entries which are weakly reachable. Entries are
 *  removed from this map when the key is no longer (strongly) referenced. This class wraps
 *  `java.util.WeakHashMap`.
 *
 *  @tparam A      type of keys contained in this map
 *  @tparam B      type of values associated with the keys
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
class WeakHashMap[A, B] extends JMapWrapper[A, B](new java.util.WeakHashMap)
    with JMapWrapperLike[A, B, WeakHashMap, WeakHashMap[A, B]] {
  override def empty = new WeakHashMap[A, B]
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
  def newBuilder[K, V]: Builder[(K, V), WeakHashMap[K,V]] = new GrowableBuilder(WeakHashMap.empty[K, V])
}

