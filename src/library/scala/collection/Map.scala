/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._

/**
 *  A map from keys of type `K` to values of type `V`.
 *
 *  $mapNote
 *
 *  '''Note:''' If you do not have specific implementations for `add` and `-` in mind,
 *        you might consider inheriting from `DefaultMap` instead.
 *
 *  '''Note:''' If your additions and mutations return the same kind of map as the map
 *        you are defining, you should inherit from `MapLike` as well.
 *
 *  @tparam K     the type of the keys in this map.
 *  @tparam V     the type of the values associated with keys.
 *
 *  @since 1.0
 */
trait Map[K, +V] extends Iterable[(K, V)] with GenMap[K, V] with MapLike[K, V, Map[K, V]] {
  def empty: Map[K, V] = Map.empty

  override def seq: Map[K, V] = this
}

/** $factoryInfo
 *  @define Coll `Map`
 *  @define coll map
 */
object Map extends MapFactory[Map] {
  def empty[K, V]: immutable.Map[K, V] = immutable.Map.empty

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] = new MapCanBuildFrom[K, V]

  /** An abstract shell used by { mutable, immutable }.Map but not by collection.Map
   *  because of variance issues.
   */
  abstract class WithDefault[K, +V](underlying: Map[K, V], d: K => V) extends AbstractMap[K, V] with Map[K, V] with Serializable {
    override def size               = underlying.size
    def get(key: K)                 = underlying.get(key) // removed in 2.9: orElse Some(default(key))
    def iterator                    = underlying.iterator
    override def default(key: K): V = d(key)
  }

}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends AbstractIterable[(K, V)] with Map[K, V]
