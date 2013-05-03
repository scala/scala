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

import generic._
import convert.Wrappers._

/** A hash map with references to entries which are weakly reachable. Entries are
 *  removed from this map when the key is no longer (strongly) referenced. This class wraps
 *  `java.util.WeakHashMap`.
 *
 *  @tparam A      type of keys contained in this map
 *  @tparam B      type of values associated with the keys
 *
 *  @since 2.8
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#weak_hash_maps "Scala's Collection Library overview"]]
 *  section on `Weak Hash Maps` for more information.
 *
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `WeakHashMap[A, B]` if the elements contained in the resulting collection are
 *    pairs of type `(A, B)`. This is because an implicit of type `CanBuildFrom[WeakHashMap, (A, B), WeakHashMap[A, B]]`
 *    is defined in object `WeakHashMap`. Otherwise, `That` resolves to the most specific type that doesn't have
 *    to contain pairs of type `(A, B)`, which is `Iterable`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `WeakHashMap`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
class WeakHashMap[A, B] extends JMapWrapper[A, B](new java.util.WeakHashMap)
			   with JMapWrapperLike[A, B, WeakHashMap[A, B]] {
  override def empty = new WeakHashMap[A, B]
}

/** $factoryInfo
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 */
object WeakHashMap extends MutableMapFactory[WeakHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), WeakHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: WeakHashMap[A, B] = new WeakHashMap[A, B]
}

