/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package parallel.mutable

import scala.collection.generic._
import scala.collection.parallel.{ ParIterableLike, Combiner }

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
trait ParIterable[T] extends scala.collection.GenIterable[T]
                        with scala.collection.parallel.ParIterable[T]
                        with GenericParTemplate[T, ParIterable]
                        with ParIterableLike[T, ParIterable[T], Iterable[T]]
                        with Mutable {
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable
  //protected[this] override def newBuilder = ParIterable.newBuilder[T]

  // if `mutable.ParIterableLike` is introduced, please move these methods there
  override def toIterable: ParIterable[T] = this

  override def toSeq: ParSeq[T] = toParCollection[T, ParSeq[T]](() => ParSeq.newCombiner[T])

  def seq: scala.collection.mutable.Iterable[T]
}

/** $factoryInfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]
  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]
}
