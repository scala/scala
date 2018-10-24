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

/** A trait for traversable collections that can be mutated.
 *  $traversableInfo
 *  @define mutability mutable
 */
trait Traversable[A] extends scala.collection.Traversable[A]
//                        with GenTraversable[A]
                        with GenericTraversableTemplate[A, Traversable]
                        with TraversableLike[A, Traversable[A]]
                        with Mutable {
  override def companion: GenericCompanion[Traversable] = Traversable
  override def seq: Traversable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable traversable collection
 *  @define Coll `mutable.Traversable`
 */
object Traversable extends TraversableFactory[Traversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Traversable[A]] = new ArrayBuffer
}


