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

/** A trait for all traversable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenMap[K, +V]
extends GenMapLike[K, V, GenMap[K, V]]
   with GenIterable[(K, V)]
{
  def seq: Map[K, V]

  def updated [V1 >: V](key: K, value: V1): GenMap[K, V1]
}

object GenMap extends GenMapFactory[GenMap] {
  def empty[K, V]: immutable.Map[K, V] = immutable.Map.empty

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), GenMap[K, V]] = new MapCanBuildFrom[K, V]
}
