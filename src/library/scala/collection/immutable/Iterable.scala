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
package immutable

import generic._
import mutable.Builder
import parallel.immutable.ParIterable

/** A base trait for iterable collections that are guaranteed immutable.
 *  $iterableInfo
 *
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
trait Iterable[+A] extends Traversable[A]
//                      with GenIterable[A]
                      with scala.collection.Iterable[A]
                      with GenericTraversableTemplate[A, Iterable]
                      with IterableLike[A, Iterable[A]]
                      with Parallelizable[A, ParIterable[A]]
{
  override def companion: GenericCompanion[Iterable] = Iterable
  protected[this] override def parCombiner = ParIterable.newCombiner[A] // if `immutable.IterableLike` gets introduced, please move this there!
  override def seq: Iterable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Iterable[A]] = new mutable.ListBuffer
}
