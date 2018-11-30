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

private[collection] class AnyIndexedSeqStepper[A](underlying: collection.IndexedSeqOps[A, Any, _], _i0: Int, _iN: Int)
  extends IndexedStepperBase[AnyStepper[A], AnyIndexedSeqStepper[A]](_i0, _iN)
    with AnyStepper[A] {
  def next(): A = if (hasNext) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): AnyIndexedSeqStepper[A] = new AnyIndexedSeqStepper[A](underlying, i0, half)
}

private[collection] class DoubleIndexedSeqStepper[CC <: collection.IndexedSeqOps[Double, Any, _]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[DoubleStepper, DoubleIndexedSeqStepper[CC]](_i0, _iN)
    with DoubleStepper {
  def nextDouble(): Double = if (hasNext) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): DoubleIndexedSeqStepper[CC] = new DoubleIndexedSeqStepper[CC](underlying, i0, half)
}

private[collection] class IntIndexedSeqStepper[CC <: collection.IndexedSeqOps[Int, Any, _]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, IntIndexedSeqStepper[CC]](_i0, _iN)
    with IntStepper {
  def nextInt(): Int = if (hasNext) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): IntIndexedSeqStepper[CC] = new IntIndexedSeqStepper[CC](underlying, i0, half)
}

private[collection] class LongIndexedSeqStepper[CC <: collection.IndexedSeqOps[Long, Any, _]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[LongStepper, LongIndexedSeqStepper[CC]](_i0, _iN)
    with LongStepper {
  def nextLong(): Long = if (hasNext) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  protected def semiclone(half: Int): LongIndexedSeqStepper[CC] = new LongIndexedSeqStepper[CC](underlying, i0, half)
}
