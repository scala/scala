/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.convert
package impl

import java.util.Spliterator

import scala.collection.Stepper.EfficientSplit

/** Abstracts all the generic operations of stepping over a collection
  * that has an indexable ordering but may have gaps.
  *
  * For collections that are guaranteed to not have gaps, use `IndexedStepperBase` instead.
  */
private[convert] abstract class InOrderStepperBase[Sub >: Null, Semi <: Sub](protected var i0: Int, protected var iN: Int)
extends EfficientSplit {
  /** Set `true` if the element at `i0` is known to be there.  `false` if either not known or is a gap.
    */
  protected def found: Boolean

  /** Advance `i0` over any gaps, updating internal state so `found` is correct at the new position.
    * Returns the new value of `found`.
    */
  protected def findNext(): Boolean

  protected def semiclone(half: Int): Semi

  final def hasStep: Boolean = found || findNext()

  def characteristics: Int = Spliterator.ORDERED

  def estimateSize: Long = iN - i0

  def trySplit(): Sub = {
    if (iN-1 > i0) {
      val half = (i0 + iN) >>> 1
      val ans = semiclone(half)
      i0 = half
      ans
    }
    else null
  }
}
