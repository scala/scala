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

import scala.collection._
import mutable.Builder

/** @define coll collection
 *  @define Coll `Traversable`
 *  @define factoryInfo
 *    This object provides a set of operations to create `$Coll` values.
 *    @author Martin Odersky
 *    @since  2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    @see CanBuildFrom
 *  @define bitsetCanBuildFrom
 *    The standard `CanBuildFrom` instance for bitsets.
 */
trait BitSetFactory[Coll <: BitSet with BitSetLike[Coll]] {
  def empty: Coll
  def newBuilder: Builder[Int, Coll]
  def apply(elems: Int*): Coll = (empty /: elems) (_ + _)
  def bitsetCanBuildFrom = new CanBuildFrom[Coll, Int, Coll] {
    def apply(from: Coll) = newBuilder
    def apply() = newBuilder
  }
}

