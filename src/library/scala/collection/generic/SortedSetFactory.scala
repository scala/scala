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

import mutable.{Builder, SetBuilder}
import scala.language.higherKinds

/** A template for companion objects of Set and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class SortedSetFactory[CC[A] <: SortedSet[A] with SortedSetLike[A, CC[A]]] {
  type Coll = CC[_]

  def empty[A](implicit ord: Ordering[A]): CC[A]

  def apply[A](elems: A*)(implicit ord: Ordering[A]): CC[A] = {
    if (elems isEmpty) empty
    else (newBuilder[A](ord) ++= elems).result()
  }

  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]] = new SetBuilder[A, CC[A]](empty)

  implicit def newCanBuildFrom[A](implicit ord : Ordering[A]) : CanBuildFrom[Coll, A, CC[A]] = new SortedSetCanBuildFrom()(ord)

  class SortedSetCanBuildFrom[A](implicit ord: Ordering[A]) extends CanBuildFrom[Coll, A, CC[A]] {
    private[collection] def factory = SortedSetFactory.this
    private[collection] def ordering = ord
    def apply(from: Coll) = newBuilder[A](ord)
    def apply() = newBuilder[A](ord)
  }
}
