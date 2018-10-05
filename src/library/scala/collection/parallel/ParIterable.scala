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
package collection.parallel

import scala.collection.GenIterable
import scala.collection.generic._
import scala.collection.parallel.mutable.ParArrayCombiner

/** A template trait for parallel iterable collections.
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
extends GenIterable[T]
   with GenericParTemplate[T, ParIterable]
   with ParIterableLike[T, ParIterable[T], Iterable[T]] {
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable
  //protected[this] override def newBuilder = ParIterable.newBuilder[T]

  def stringPrefix = "ParIterable"
}

/** $factoryInfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]
}

