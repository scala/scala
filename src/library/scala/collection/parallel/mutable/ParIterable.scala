/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel.mutable


import scala.collection.generic._

import scala.collection.parallel.ParIterableLike
import scala.collection.parallel.Combiner


/** A template trait for mutable parallel iterable collections.
 *
 *  $paralleliterableinfo
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the collection
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParIterable[T] extends collection.mutable.Iterable[T]
                        with collection.parallel.ParIterable[T]
                        with GenericParTemplate[T, ParIterable]
                        with ParIterableLike[T, ParIterable[T], Iterable[T]] {
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable

  // if `mutable.ParIterableLike` is introduced, please move these 4 methods there
  override def toIterable: ParIterable[T] = this

  override def toSeq: ParSeq[T] = toParCollection[T, ParSeq[T]](() => ParSeq.newCombiner[T])
}

/** $factoryinfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] =
    new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]
}














