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

import parallel.mutable.ParSeq

/** A template trait for mutable sequences of type `mutable.Seq[A]`.
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 */
trait SeqLike[A, +This <: SeqLike[A, This] with Seq[A]]
  extends scala.collection.SeqLike[A, This]
     with Cloneable[This]
     with Parallelizable[A, ParSeq[A]]
{
  self =>

  protected[this] override def parCombiner = ParSeq.newCombiner[A]

  /** Replaces element at given index with a new value.
   *
   *  @param idx      the index of the element to replace.
   *  @param elem     the new value.
   *  @throws   IndexOutOfBoundsException if the index is not valid.
   */
  def update(idx: Int, elem: A)

  /** Applies a transformation function to all values contained in this sequence.
   *  The transformation function produces new values from existing elements.
   *
   * @param f  the transformation to apply
   * @return   the sequence itself.
   */
  def transform(f: A => A): this.type = {
    var i = 0
    this foreach { el =>
      this(i) = f(el)
      i += 1
    }
    this
  }
}
