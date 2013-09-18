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
trait GenMap[A, +B]
extends GenMapLike[A, B, GenMap[A, B]]
   with GenIterable[(A, B)]
{
  def seq: Map[A, B]

  def updated [B1 >: B](key: A, value: B1): GenMap[A, B1]
}

object GenMap extends GenMapFactory[GenMap] {
  def empty[A, B]: immutable.Map[A, B] = immutable.Map.empty

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), GenMap[A, B]] = new MapCanBuildFrom[A, B]
}
