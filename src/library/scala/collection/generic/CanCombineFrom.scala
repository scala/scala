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

import scala.collection.parallel._

/** A base trait for parallel builder factories.
 *
 *  @tparam From  the type of the underlying collection that requests a
 *                builder to be created.
 *  @tparam Elem  the element type of the collection to be created.
 *  @tparam To    the type of the collection to be created.
 *  @since 2.8
 */
trait CanCombineFrom[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] with Parallel {
  def apply(from: From): Combiner[Elem, To]
  def apply(): Combiner[Elem, To]
}

