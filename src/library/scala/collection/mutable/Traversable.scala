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
import scala.collection.mutable.Iterable

/** A trait for traversable collections that can be mutated.
 *  $traversableInfo
 *  @define mutability mutable
 */
trait Iterable[A] extends scala.collection.Iterable[A]
//                        with GenTraversable[A]
                        with GenericTraversableTemplate[A, Iterable]
                        with TraversableLike[A, Iterable[A]]
                        with Mutable {
  override def companion: GenericCompanion[Iterable] = Iterable
  override def seq: Iterable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable traversable collection
 *  @define Coll `mutable.Traversable`
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Iterable[A]] = new ArrayBuffer
}


