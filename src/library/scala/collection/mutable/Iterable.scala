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
import parallel.mutable.ParIterable

/** A base trait for iterable collections that can be mutated.
 *  $iterableInfo
 */
trait Iterable[A] extends Traversable[A]
//                     with GenIterable[A]
                     with scala.collection.Iterable[A]
                     with GenericTraversableTemplate[A, Iterable]
                     with IterableLike[A, Iterable[A]]
                     with Parallelizable[A, ParIterable[A]]
{
  override def companion: GenericCompanion[Iterable] = Iterable
  protected[this] override def parCombiner = ParIterable.newCombiner[A] // if `mutable.IterableLike` gets introduced, please move this there!
  override def seq: Iterable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable iterable collection
 *  @define Coll `mutable.Iterable`
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Iterable[A]] = new ArrayBuffer
}

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with Iterable[A]
