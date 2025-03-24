/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection.mutable


/** The canonical builder for collections that are growable, i.e. that support an
  * efficient `+=` method which adds an element to the collection.
  *
  * GrowableBuilders can produce only a single instance of the collection they are growing.
  *
  * @define Coll `GrowingBuilder`
  * @define coll growing builder
  */
class GrowableBuilder[Elem, To <: Growable[Elem]](protected val elems: To)
  extends Builder[Elem, To] {

  def clear(): Unit = elems.clear()

  def result(): To = elems

  def addOne(elem: Elem): this.type = { elems += elem; this }

  override def addAll(xs: IterableOnce[Elem]): this.type = { elems.addAll(xs); this }

  override def knownSize: Int = elems.knownSize
}
