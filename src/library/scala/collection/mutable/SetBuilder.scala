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
package mutable

/** The canonical builder for mutable Sets.
 *
 *  @tparam A      The type of the elements that will be contained in this set.
 *  @tparam Coll   The type of the actual collection this set builds.
 *  @param empty   The empty element of the collection.
 *  @since 2.8
 */
class SetBuilder[A, Coll <: scala.collection.Set[A]
with scala.collection.SetLike[A, Coll]](empty: Coll)
extends ReusableBuilder[A, Coll] {
  protected var elems: Coll = empty
  def +=(x: A): this.type = { elems = elems + x; this }
  def clear() { elems = empty }
  def result: Coll = elems
}
