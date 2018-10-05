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
package generic

import scala.collection.parallel.Combiner
import scala.collection.parallel.ParSet
import scala.collection.parallel.ParSetLike
import scala.language.higherKinds

/**
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
abstract class ParSetFactory[CC[X] <: ParSet[X] with ParSetLike[X, CC[X], _] with GenericParTemplate[X, CC]]
  extends GenSetFactory[CC]
     with GenericParCompanion[CC]
{
  def newBuilder[A]: Combiner[A, CC[A]] = newCombiner[A]

  def newCombiner[A]: Combiner[A, CC[A]]

  class GenericCanCombineFrom[A] extends CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner[A]
    override def apply() = newCombiner[A]
  }
}

