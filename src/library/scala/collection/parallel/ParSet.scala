/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package parallel

import scala.collection.generic._

/** A template trait for parallel sets.
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the set
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParSet[T]
   extends GenSet[T]
   with GenericParTemplate[T, ParSet]
   with ParIterable[T]
   with ParSetLike[T, ParSet[T], Set[T]]
{ self =>

  override def empty: ParSet[T] = mutable.ParHashSet[T]()

  //protected[this] override def newCombiner: Combiner[T, ParSet[T]] = ParSet.newCombiner[T]

  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet

  override def stringPrefix = "ParSet"
}

object ParSet extends ParSetFactory[ParSet] {
  def newCombiner[T]: Combiner[T, ParSet[T]] = mutable.ParHashSetCombiner[T]

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]
}
