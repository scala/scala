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

import scala.collection.{AnyStepper, IntStepper, LongStepper, Stepper}
import scala.collection.immutable.NumericRange

private[collection] class AnyNumericRangeStepper[A](underlying: NumericRange[A], _i0: Int, _iN: Int)
extends IndexedStepperBase[AnyStepper[A], AnyNumericRangeStepper[A]](_i0, _iN)
with AnyStepper[A] {
  def nextStep(): A = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  def semiclone(half: Int) = new AnyNumericRangeStepper[A](underlying, i0, half)
}

private[collection] class IntNumericRangeStepper(underlying: NumericRange[Int], _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, IntNumericRangeStepper](_i0, _iN)
with IntStepper {
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  def semiclone(half: Int) = new IntNumericRangeStepper(underlying, i0, half)
}

private[collection] class LongNumericRangeStepper(underlying: NumericRange[Long], _i0: Int, _iN: Int)
extends IndexedStepperBase[LongStepper, LongNumericRangeStepper](_i0, _iN)
with LongStepper {
  def nextStep(): Long = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  def semiclone(half: Int) = new LongNumericRangeStepper(underlying, i0, half)
}
