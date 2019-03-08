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

package scala.collection.convert
package impl

import java.lang.Character.{charCount, isLowSurrogate}

/** Implements `Stepper` on a `String` where you step through chars packed into `Int`.
  */
private[collection] final class CharStringStepper(underlying: String, _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, CharStringStepper](_i0, _iN)
with IntStepper {
  def nextInt(): Int =
    if (hasNext) { val j = i0; i0 += 1; underlying.charAt(j) }
    else Stepper.throwNSEE()

  def semiclone(half: Int): CharStringStepper = new CharStringStepper(underlying, i0, half)
}

/** Implements `Stepper` on a `String` where you step through code points.
  */
private[collection] final class CodePointStringStepper(underlying: String, private var i0: Int, private var iN: Int)
extends IntStepper with EfficientSubstep {
  def characteristics(): Int = Stepper.Immutable | Stepper.NonNull | Stepper.Ordered
  def estimateSize: Long = iN - i0
  def hasNext: Boolean = i0 < iN
  def nextInt(): Int = {
    if (hasNext) {
      val cp = underlying.codePointAt(i0)
      i0 += charCount(cp)
      cp
    }
    else Stepper.throwNSEE()
  }
  def substep(): CodePointStringStepper =
    if (iN - 3 > i0) {
      var half = (i0 + iN) >>> 1
      if (isLowSurrogate(underlying.charAt(half))) half -= 1
      val ans = new CodePointStringStepper(underlying, i0, half)
      i0 = half
      ans
    }
    else null
}
