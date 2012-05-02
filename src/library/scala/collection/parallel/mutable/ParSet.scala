/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel.mutable



import scala.collection.generic._
import scala.collection.parallel.Combiner
import scala.collection.GenSet





/** A mutable variant of `ParSet`.
 *
 *  @define Coll `mutable.ParSet`
 *  @define coll mutable parallel set
 *
 *  @author Aleksandar Prokopec
 */
trait ParSet[T]
extends collection/*.mutable*/.GenSet[T]
   with ParIterable[T]
   with collection.parallel.ParSet[T]
   with GenericParTemplate[T, ParSet]
   with ParSetLike[T, ParSet[T], collection.mutable.Set[T]]
{
self =>
  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet
  override def empty: ParSet[T] = ParHashSet()
  def seq: collection.mutable.Set[T]
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

