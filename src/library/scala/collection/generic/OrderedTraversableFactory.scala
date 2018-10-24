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

import scala.language.higherKinds

abstract class OrderedTraversableFactory[CC[X] <: Traversable[X] with GenericOrderedTraversableTemplate[X, CC]]
extends GenericOrderedCompanion[CC] {

  class GenericCanBuildFrom[A](implicit ord: Ordering[A]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = from.genericOrderedBuilder[A]
    def apply = newBuilder[A]
  }

}
