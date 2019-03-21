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

package scala.collection

import scala.collection.Stepper.EfficientSplit

/** An implicit StepperShape instance is used in the [[IterableOnce.stepper]] to return a possibly
  * specialized Stepper `S` according to the element type `T`.
  */
sealed trait StepperShape[T, S <: Stepper[_]] {
  /** Return the Int constant (as defined in the `StepperShape` companion object) for this `StepperShape`. */
  def shape: Int

  /** Create an unboxing primitive sequential Stepper from a boxed `AnyStepper`.
   * This is an identity operation for reference shapes. */
  def seqUnbox(st: AnyStepper[T]): S

  /** Create an unboxing primitive parallel (i.e. `with EfficientSubstep`) Stepper from a boxed `AnyStepper`.
   * This is an identity operation for reference shapes. */
  def parUnbox(st: AnyStepper[T] with EfficientSplit): S with EfficientSplit
}

object StepperShape extends StepperShapeLowPriority {
  // reference
  final val Reference   = 0

  // primitive
  final val IntValue    = 1
  final val LongValue   = 2
  final val DoubleValue = 3

  // widening
  final val ByteValue   = 4
  final val ShortValue  = 5
  final val CharValue   = 6
  final val FloatValue  = 7

  implicit val intStepperShape: StepperShape[Int, IntStepper] = new StepperShape[Int, IntStepper] {
    def shape = IntValue
    def seqUnbox(st: AnyStepper[Int]): IntStepper = new Stepper.UnboxingIntStepper(st)
    def parUnbox(st: AnyStepper[Int] with EfficientSplit): IntStepper with EfficientSplit = new Stepper.UnboxingIntStepper(st) with EfficientSplit
  }
  implicit val longStepperShape: StepperShape[Long, LongStepper] = new StepperShape[Long, LongStepper] {
    def shape = LongValue
    def seqUnbox(st: AnyStepper[Long]): LongStepper = new Stepper.UnboxingLongStepper(st)
    def parUnbox(st: AnyStepper[Long] with EfficientSplit): LongStepper with EfficientSplit = new Stepper.UnboxingLongStepper(st) with EfficientSplit
  }
  implicit val doubleStepperShape: StepperShape[Double, DoubleStepper] = new StepperShape[Double, DoubleStepper] {
    def shape = DoubleValue
    def seqUnbox(st: AnyStepper[Double]): DoubleStepper = new Stepper.UnboxingDoubleStepper(st)
    def parUnbox(st: AnyStepper[Double] with EfficientSplit): DoubleStepper with EfficientSplit = new Stepper.UnboxingDoubleStepper(st) with EfficientSplit
  }
  implicit val byteStepperShape: StepperShape[Byte, IntStepper] = new StepperShape[Byte, IntStepper] {
    def shape = ByteValue
    def seqUnbox(st: AnyStepper[Byte]): IntStepper = new Stepper.UnboxingByteStepper(st)
    def parUnbox(st: AnyStepper[Byte] with EfficientSplit): IntStepper with EfficientSplit = new Stepper.UnboxingByteStepper(st) with EfficientSplit
  }
  implicit val shortStepperShape: StepperShape[Short, IntStepper] = new StepperShape[Short, IntStepper] {
    def shape = ShortValue
    def seqUnbox(st: AnyStepper[Short]): IntStepper = new Stepper.UnboxingShortStepper(st)
    def parUnbox(st: AnyStepper[Short] with EfficientSplit): IntStepper with EfficientSplit = new Stepper.UnboxingShortStepper(st) with EfficientSplit
  }
  implicit val charStepperShape: StepperShape[Char, IntStepper] = new StepperShape[Char, IntStepper] {
    def shape = CharValue
    def seqUnbox(st: AnyStepper[Char]): IntStepper = new Stepper.UnboxingCharStepper(st)
    def parUnbox(st: AnyStepper[Char] with EfficientSplit): IntStepper with EfficientSplit = new Stepper.UnboxingCharStepper(st) with EfficientSplit
  }
  implicit val floatStepperShape: StepperShape[Float, DoubleStepper] = new StepperShape[Float, DoubleStepper] {
    def shape = FloatValue
    def seqUnbox(st: AnyStepper[Float]): DoubleStepper = new Stepper.UnboxingFloatStepper(st)
    def parUnbox(st: AnyStepper[Float] with EfficientSplit): DoubleStepper with EfficientSplit = new Stepper.UnboxingFloatStepper(st) with EfficientSplit
  }
}

trait StepperShapeLowPriority {
  implicit def anyStepperShape[T] = anyStepperShapePrototype.asInstanceOf[StepperShape[T, AnyStepper[T]]]

  private[this] val anyStepperShapePrototype: StepperShape[AnyRef, AnyStepper[AnyRef]] = new StepperShape[AnyRef, AnyStepper[AnyRef]] {
    def shape = StepperShape.Reference
    def seqUnbox(st: AnyStepper[AnyRef]): AnyStepper[AnyRef] = st
    def parUnbox(st: AnyStepper[AnyRef] with EfficientSplit): AnyStepper[AnyRef] with EfficientSplit = st
  }
}
