/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.Map
import scala.collection.GenMap
import scala.collection.generic.ParMapFactory
import scala.collection.generic.GenericParMapTemplate
import scala.collection.generic.GenericParMapCompanion
import scala.collection.generic.CanCombineFrom

/** A template trait for parallel maps.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMap[K, +V]
extends GenMap[K, V]
   with GenericParMapTemplate[K, V, ParMap]
   with ParIterable[(K, V)]
   with ParMapLike[K, V, ParMap[K, V], Map[K, V]]
{
self =>

  def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  //protected[this] override def newCombiner: Combiner[(K, V), ParMap[K, V]] = ParMap.newCombiner[K, V]

  def empty: ParMap[K, V] = new mutable.ParHashMap[K, V]

  override def stringPrefix = "ParMap"

  override def updated [U >: V](key: K, value: U): ParMap[K, U] = this + ((key, value))

  def + [U >: V](kv: (K, U)): ParMap[K, U]
}



object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new mutable.ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = mutable.ParHashMapCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

  /** An abstract shell used by { mutable, immutable }.Map but not by collection.Map
   *  because of variance issues.
   */
  abstract class WithDefault[A, +B](underlying: ParMap[A, B], d: A => B) extends ParMap[A, B] {
    override def size               = underlying.size
    def get(key: A)                 = underlying.get(key)
    def splitter                    = underlying.splitter
    override def default(key: A): B = d(key)
  }
}
