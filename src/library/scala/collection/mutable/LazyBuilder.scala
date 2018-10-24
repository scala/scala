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

/** A builder that constructs its result lazily. Iterators or iterables to
 *  be added to this builder with `++=` are not evaluated until `result` is called.
 *
 *  This builder can be reused.
 *
 *  @since 2.8
 *
 *  @tparam Elem    type of the elements for this builder.
 *  @tparam To      type of the collection this builder builds.
 */
abstract class LazyBuilder[Elem, +To] extends ReusableBuilder[Elem, To] {
  /** The different segments of elements to be added to the builder, represented as iterators */
  protected var parts = new ListBuffer[TraversableOnce[Elem]]
  def +=(x: Elem): this.type = { parts += List(x); this }
  override def ++=(xs: TraversableOnce[Elem]): this.type = { parts += xs ; this }
  def result(): To
  def clear() { parts.clear() }
}
