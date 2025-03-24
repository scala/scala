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

import java.lang.Character.{charCount, isLowSurrogate}
import java.util.Spliterator

import scala.collection.Stepper.EfficientSplit
import scala.collection.{IntStepper, Stepper}

/** Implements `Stepper` on a `String` where you step through chars packed into `Int`.
  */
private[collection] final class CharStringStepper(underlying: String, _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, CharStringStepper](_i0, _iN)
with IntStepper {
  def nextStep(): Int =
    if (hasStep) { val j = i0; i0 += 1; underlying.charAt(j) }
    else Stepper.throwNSEE()

  def semiclone(half: Int): CharStringStepper = new CharStringStepper(underlying, i0, half)
}

/** Implements `Stepper` on a `String` where you step through code points.
  */
private[collection] final class CodePointStringStepper(underlying: String, private var i0: Int, private var iN: Int)
extends IntStepper with EfficientSplit {
  def characteristics: Int = Spliterator.IMMUTABLE | Spliterator.NONNULL | Spliterator.ORDERED
  def estimateSize: Long = iN - i0
  def hasStep: Boolean = i0 < iN
  def nextStep(): Int = {
    if (hasStep) {
      val cp = underlying.codePointAt(i0)
      i0 += charCount(cp)
      cp
    }
    else Stepper.throwNSEE()
  }
  def trySplit(): CodePointStringStepper =
    if (iN - 3 > i0) {
      var half = (i0 + iN) >>> 1
      if (isLowSurrogate(underlying.charAt(half))) half -= 1
      val ans = new CodePointStringStepper(underlying, i0, half)
      i0 = half
      ans
    }
    else null
}
