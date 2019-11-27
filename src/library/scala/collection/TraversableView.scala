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

import generic._
import mutable.Builder

/** A base trait for non-strict views of traversable collections.
 *  $traversableViewInfo
 */
trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]] { }

/** An object containing the necessary implicit definitions to make
 *  `TraversableView`s work. Its definitions are generally not accessed directly by clients.
 */
object TraversableView {
  class NoBuilder[A] extends Builder[A, Nothing] {
    def +=(elem: A): this.type = this
    def iterator: Iterator[A] = Iterator.empty
    def result() = throw new UnsupportedOperationException("TraversableView.Builder.result")
    def clear() {}
  }
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] =
    new CanBuildFrom[Coll, A, TraversableView[A, Traversable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
