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
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

/** This trait represents collections classes which require
 *  ordered element types.
 *
 *  @author Aleksandar Prokopec
 */
trait GenericOrderedTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {
  implicit protected[this] val ord: Ordering[A]
  def orderedCompanion: GenericOrderedCompanion[CC]
  def genericOrderedBuilder[B](implicit ord: Ordering[B]): Builder[B, CC[B]] = orderedCompanion.newBuilder[B]
}

