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

import scala.collection._

private[collection] class ObjectArrayStepper[A <: Object](underlying: Array[A], _i0: Int, _iN: Int)
  extends IndexedStepperBase[AnyStepper[A], ObjectArrayStepper[A]](_i0, _iN)
    with AnyStepper[A] {
  def nextStep(): A = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): ObjectArrayStepper[A] = new ObjectArrayStepper[A](underlying, i0, half)
}

private[collection] class BoxedBooleanArrayStepper(underlying: Array[Boolean], _i0: Int, _iN: Int)
  extends IndexedStepperBase[AnyStepper[Boolean], BoxedBooleanArrayStepper](_i0, _iN)
    with AnyStepper[Boolean] {
  def nextStep(): Boolean = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): BoxedBooleanArrayStepper = new BoxedBooleanArrayStepper(underlying, i0, half)
}

private[collection] class WidenedByteArrayStepper(underlying: Array[Byte], _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, WidenedByteArrayStepper](_i0, _iN)
    with IntStepper {
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): WidenedByteArrayStepper = new WidenedByteArrayStepper(underlying, i0, half)
}

private[collection] class WidenedCharArrayStepper(underlying: Array[Char], _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, WidenedCharArrayStepper](_i0, _iN)
    with IntStepper {
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): WidenedCharArrayStepper = new WidenedCharArrayStepper(underlying, i0, half)
}

private[collection] class WidenedShortArrayStepper(underlying: Array[Short], _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, WidenedShortArrayStepper](_i0, _iN)
    with IntStepper {
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): WidenedShortArrayStepper = new WidenedShortArrayStepper(underlying, i0, half)
}

private[collection] class WidenedFloatArrayStepper(underlying: Array[Float], _i0: Int, _iN: Int)
  extends IndexedStepperBase[DoubleStepper, WidenedFloatArrayStepper](_i0, _iN)
    with DoubleStepper {
  def nextStep(): Double = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): WidenedFloatArrayStepper = new WidenedFloatArrayStepper(underlying, i0, half)
}

private[collection] class DoubleArrayStepper(underlying: Array[Double], _i0: Int, _iN: Int)
  extends IndexedStepperBase[DoubleStepper, DoubleArrayStepper](_i0, _iN)
    with DoubleStepper {
  def nextStep(): Double = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): DoubleArrayStepper = new DoubleArrayStepper(underlying, i0, half)
}

private[collection] class IntArrayStepper(underlying: Array[Int], _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, IntArrayStepper](_i0, _iN)
    with IntStepper {
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): IntArrayStepper = new IntArrayStepper(underlying, i0, half)
}

private[collection] class LongArrayStepper(underlying: Array[Long], _i0: Int, _iN: Int)
  extends IndexedStepperBase[LongStepper, LongArrayStepper](_i0, _iN)
    with LongStepper {
  def nextStep(): Long = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): LongArrayStepper = new LongArrayStepper(underlying, i0, half)
}
