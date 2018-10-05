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

import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParCompanion
import scala.collection.generic.GenericParTemplate
import scala.collection.generic.ParFactory
import scala.collection.generic.CanCombineFrom
import scala.collection.GenSeq
import scala.collection.parallel.mutable.ParArrayCombiner

/** A template trait for parallel sequences.
 *
 *  $parallelseqinfo
 *
 *  $sideeffects
 *
 *  @tparam T      the type of the elements in this parallel sequence
 *
 *  @author Aleksandar Prokopec
 */
trait ParSeq[+T] extends GenSeq[T]
                    with ParIterable[T]
                    with GenericParTemplate[T, ParSeq]
                    with ParSeqLike[T, ParSeq[T], Seq[T]]
{
  override def companion: GenericCompanion[ParSeq] with GenericParCompanion[ParSeq] = ParSeq
  //protected[this] override def newBuilder = ParSeq.newBuilder[T]

  def apply(i: Int): T

  override def toString = super[ParIterable].toString

  override def stringPrefix = getClass.getSimpleName
}

object ParSeq extends ParFactory[ParSeq] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSeq[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParSeq[T]] = ParArrayCombiner[T]
  def newCombiner[T]: Combiner[T, ParSeq[T]] = ParArrayCombiner[T]
}
