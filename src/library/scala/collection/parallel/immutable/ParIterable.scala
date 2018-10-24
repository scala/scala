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
package parallel.immutable

import scala.collection.generic._
import scala.collection.parallel.ParIterableLike
import scala.collection.parallel.Combiner

/** A template trait for immutable parallel iterable collections.
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
trait ParIterable[+T]
extends scala.collection.GenIterable[T]
   with scala.collection.parallel.ParIterable[T]
   with GenericParTemplate[T, ParIterable]
   with ParIterableLike[T, ParIterable[T], scala.collection.immutable.Iterable[T]]
   with Immutable
{
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable
  // if `immutable.ParIterableLike` is introduced, please move these 4 methods there
  override def toIterable: ParIterable[T] = this
  override def toSeq: ParSeq[T] = toParCollection[T, ParSeq[T]](() => ParSeq.newCombiner[T])
}

/** $factoryInfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] =
    new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParVector.newBuilder[T]
  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParVector.newCombiner[T]
}
