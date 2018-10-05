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

import scala.collection.parallel.ParIterable
import scala.language.higherKinds

/** A template class for companion objects of `ParIterable` and subclasses
 *  thereof. This class extends `TraversableFactory` and provides a set of
 *  operations to create `$Coll` objects.
 *
 *  @define coll parallel collection
 *  @define Coll `ParIterable`
 *  @since 2.8
 */
abstract class ParFactory[CC[X] <: ParIterable[X] with GenericParTemplate[X, CC]]
extends GenTraversableFactory[CC]
   with GenericParCompanion[CC] {

  //type EPC[T, C] = scala.collection.parallel.EnvironmentPassingCombiner[T, C]

  /** A generic implementation of the `CanCombineFrom` trait, which forwards
   *  all calls to `apply(from)` to the `genericParBuilder` method of the $coll
   * `from`, and calls to `apply()` to this factory.
   */
  class GenericCanCombineFrom[A] extends GenericCanBuildFrom[A] with CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner
    override def apply() = newBuilder[A]
  }
}
