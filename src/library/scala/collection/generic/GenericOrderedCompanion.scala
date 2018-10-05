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
import scala.language.higherKinds

/** This class represents companions of classes which require the ordered trait
 *  for their element types.
 *
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
abstract class GenericOrderedCompanion[+CC[X] <: Traversable[X]] {
  protected[this] type Coll = CC[_]

  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]]

  def empty[A: Ordering]: CC[A] = newBuilder[A].result()

  def apply[A](elems: A*)(implicit ord: Ordering[A]): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result()
  }
}

