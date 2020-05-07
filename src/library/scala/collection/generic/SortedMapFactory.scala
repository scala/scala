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

import mutable.{Builder, MapBuilder}
import scala.language.higherKinds

/** A template for companion objects of mutable.Map and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class SortedMapFactory[CC[A, B] <: SortedMap[A, B] with SortedMapLike[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def empty[A, B](implicit ord: Ordering[A]): CC[A, B]

  def apply[A, B](elems: (A, B)*)(implicit ord: Ordering[A]): CC[A, B] = {
    if (elems.isEmpty) empty
    else (newBuilder[A, B](ord) ++= elems).result()
  }

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]] =
    new MapBuilder[A, B, CC[A, B]](empty(ord))

  class SortedMapCanBuildFrom[A, B](implicit ord: Ordering[A]) extends CanBuildFrom[Coll, (A, B), CC[A, B]] {
    private[collection] def factory = SortedMapFactory.this
    private[collection] def ordering = ord
    def apply(from: Coll) = newBuilder[A, B](ord)
    def apply() = newBuilder[A, B]
  }
}
