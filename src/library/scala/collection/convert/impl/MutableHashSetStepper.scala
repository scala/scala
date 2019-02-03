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


private[collection] final class AnyMutableHashSetStepper[A, I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => A, _i0: Int, _iN: Int
)
extends TableStepperBase[A, I, AnyStepper[A], AnyMutableHashSetStepper[A, I]](_maxLength, _table, _i0, _iN)
with AnyStepper[A] {
  def next() = 
    if (hasNext) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE

  def semiclone(half: Int) = new AnyMutableHashSetStepper[A, I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class DoubleMutableHashSetStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Double, _i0: Int, _iN: Int
)
extends TableStepperBase[Double, I, DoubleStepper, DoubleMutableHashSetStepper[I]](_maxLength, _table, _i0, _iN)
with DoubleStepper {
  def nextDouble() = 
    if (hasNext) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE

  def semiclone(half: Int) = new DoubleMutableHashSetStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class IntMutableHashSetStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Int, _i0: Int, _iN: Int
)
extends TableStepperBase[Int, I, IntStepper, IntMutableHashSetStepper[I]](_maxLength, _table, _i0, _iN)
with IntStepper {
  def nextInt() = 
    if (hasNext) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE

  def semiclone(half: Int) = new IntMutableHashSetStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class LongMutableHashSetStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Long, _i0: Int, _iN: Int
)
extends TableStepperBase[Long, I, LongStepper, LongMutableHashSetStepper[I]](_maxLength, _table, _i0, _iN)
with LongStepper {
  def nextLong() = 
    if (hasNext) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE

  def semiclone(half: Int) = new LongMutableHashSetStepper[I](maxLength, table, iterate, extract, i0, half)
}
