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

import generic._

/** A trait for all traversable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenTraversable[+A]
extends GenTraversableLike[A, GenTraversable[A]]
   with GenTraversableOnce[A]
   with GenericTraversableTemplate[A, GenTraversable]
{
  def seq: Traversable[A]
  def companion: GenericCompanion[GenTraversable] = GenTraversable
}

object GenTraversable extends GenTraversableFactory[GenTraversable] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = Traversable.newBuilder
}
