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

import generic._

/** The canonical builder for collections that are growable, i.e. that support an
 *  efficient `+=` method which adds an element to the collection.
 *
 *  GrowableBuilders can produce only a single instance of the collection they are growing.
 *
 *  @author Paul Phillips
 *  @since 2.8
 *
 *  @define Coll `GrowingBuilder`
 *  @define coll growing builder
 */
class GrowingBuilder[Elem, To <: Growable[Elem]](empty: To) extends Builder[Elem, To] {
  protected var elems: To = empty
  def +=(x: Elem): this.type = { elems += x; this }
  def clear() { empty.clear }
  def result: To = elems
}
