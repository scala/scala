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
package parallel.mutable

import scala.collection.mutable.Cloneable
import scala.collection.GenSetLike
import scala.collection.generic.Growable
import scala.collection.generic.Shrinkable

/** A template trait for mutable parallel sets. This trait is mixed in with concrete
 *  parallel sets to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the set
 *  @define Coll `mutable.ParSet`
 *  @define coll mutable parallel set
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParSetLike[T,
                 +Repr <: ParSetLike[T, Repr, Sequential] with ParSet[T],
                 +Sequential <: mutable.Set[T] with mutable.SetLike[T, Sequential]]
extends GenSetLike[T, Repr]
   with scala.collection.parallel.ParIterableLike[T, Repr, Sequential]
   with scala.collection.parallel.ParSetLike[T, Repr, Sequential]
   with Growable[T]
   with Shrinkable[T]
   with Cloneable[Repr]
{
self =>
  override def empty: Repr

  def +=(elem: T): this.type

  def -=(elem: T): this.type

  def +(elem: T) = this.clone() += elem

  def -(elem: T) = this.clone() -= elem

  // note: should not override toSet
}
