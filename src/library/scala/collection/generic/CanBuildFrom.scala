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

import mutable.Builder
import scala.annotation.implicitNotFound

/** A base trait for builder factories.
 *
 *  @tparam From  the type of the underlying collection that requests
 *                a builder to be created.
 *  @tparam Elem  the element type of the collection to be created.
 *  @tparam To    the type of the collection to be created.
 *
 *  @see [[scala.collection.mutable.Builder]]
 *  @author Martin Odersky
 *  @author Adriaan Moors
 *  @since 2.8
 */
@implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.")
trait CanBuildFrom[-From, -Elem, +To] {

  /** Creates a new builder on request of a collection.
   *  @param from  the collection requesting the builder to be created.
   *  @return a builder for collections of type `To` with element type `Elem`.
   *          The collections framework usually arranges things so
   *          that the created builder will build the same kind of collection
   *          as `from`.
   */
  def apply(from: From): Builder[Elem, To]

  /** Creates a new builder from scratch.
   *
   *  @return a builder for collections of type `To` with element type `Elem`.
   *  @see scala.collection.breakOut
   */
  def apply(): Builder[Elem, To]
}
