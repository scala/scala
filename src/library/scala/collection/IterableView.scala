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
import TraversableView.NoBuilder

/** A base trait for non-strict views of `Iterable`s.
 *  $iterableViewInfo
 */
trait IterableView[+A, +Coll] extends IterableViewLike[A, Coll, IterableView[A, Coll]]

/** An object containing the necessary implicit definitions to make
 *  `IterableView`s work. Its definitions are generally not accessed directly by clients.
 */
object IterableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] =
    new CanBuildFrom[Coll, A, IterableView[A, Iterable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
