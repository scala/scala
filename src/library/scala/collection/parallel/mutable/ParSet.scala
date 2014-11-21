/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel.mutable

import scala.collection.generic._
import scala.collection.parallel.Combiner

/** A mutable variant of `ParSet`.
 *
 *  @author Aleksandar Prokopec
 */
trait ParSet[T]
extends scala.collection/*.mutable*/.GenSet[T]
   with ParIterable[T]
   with scala.collection.parallel.ParSet[T]
   with GenericParTemplate[T, ParSet]
   with ParSetLike[T, ParSet[T], scala.collection.mutable.Set[T]]
{
self =>
  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet
  override def empty: ParSet[T] = ParHashSet()
  def seq: scala.collection.mutable.Set[T]
}


/** $factoryInfo
 *  @define Coll `mutable.ParSet`
 *  @define coll mutable parallel set
 */
object ParSet extends ParSetFactory[ParSet] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]

  override def newBuilder[T]: Combiner[T, ParSet[T]] = ParHashSet.newBuilder

  override def newCombiner[T]: Combiner[T, ParSet[T]] = ParHashSet.newCombiner
}
