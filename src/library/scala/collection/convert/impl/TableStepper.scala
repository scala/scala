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

import scala.collection.Stepper.EfficientSplit
import scala.collection._

private[collection] abstract class TableStepperBase[A, I >: Null <: AnyRef, Sub >: Null, Semi <: Sub with TableStepperBase[A, I, _, _]](
  protected var maxLength: Int, protected val table: Array[I], protected var i0: Int, protected val iN: Int
)
extends EfficientSplit {
  // Always holds table(i0); if `null` it is time to switch to the next element
  protected var myCurrent: I = if (i0 < iN) table(i0) else null

  // Only call this when `myCurrent` is null (meaning we need to advance)
  @annotation.tailrec
  protected final def findNextCurrent(): Boolean =
    if (i0 < iN) {
      i0 += 1
      if (i0 >= iN) false
      else {
        myCurrent = table(i0)
        if (myCurrent eq null) findNextCurrent()
        else true
      }
    }
    else false

  protected def semiclone(half: Int): Semi

  def characteristics: Int = 0

  def estimateSize: Long = if (!hasStep) { maxLength = 0; 0 } else maxLength

  def hasStep: Boolean = (myCurrent ne null) || findNextCurrent()

  def trySplit(): Sub = {
    if (iN-1 > i0 && maxLength > 0) {
      val half = (i0 + iN) >>> 1
      val ans = semiclone(half)
      ans.myCurrent = myCurrent
      myCurrent = table(half)
      var inLeft = if (ans.myCurrent ne null) 1 else 0
      var inRight = if (myCurrent ne null) 1 else 0
      if (iN - i0 < 32) {
        var i = i0+1
        while (i < half && (table(i) ne null)) { i += 1; inLeft += 1 }
        i = half+1
        while (i < iN && (table(i) ne null)) { i += 1; inRight += 1 }
      }
      maxLength -= inLeft
      ans.maxLength -= inRight
      i0 = half
      ans
    }
    else null
  }
}


private[collection] final class AnyTableStepper[A, I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => A, _i0: Int, _iN: Int
)
extends TableStepperBase[A, I, AnyStepper[A], AnyTableStepper[A, I]](_maxLength, _table, _i0, _iN)
with AnyStepper[A] {
  def nextStep(): A =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(half: Int): AnyTableStepper[A, I] = new AnyTableStepper[A, I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class DoubleTableStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Double, _i0: Int, _iN: Int
)
extends TableStepperBase[Double, I, DoubleStepper, DoubleTableStepper[I]](_maxLength, _table, _i0, _iN)
with DoubleStepper {
  def nextStep(): Double =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(half: Int): DoubleTableStepper[I] = new DoubleTableStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class IntTableStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Int, _i0: Int, _iN: Int
)
extends TableStepperBase[Int, I, IntStepper, IntTableStepper[I]](_maxLength, _table, _i0, _iN)
with IntStepper {
  def nextStep(): Int =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(half: Int): IntTableStepper[I] = new IntTableStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class LongTableStepper[I >: Null <: AnyRef](
  _maxLength: Int, _table: Array[I], iterate: I => I, extract: I => Long, _i0: Int, _iN: Int
)
extends TableStepperBase[Long, I, LongStepper, LongTableStepper[I]](_maxLength, _table, _i0, _iN)
with LongStepper {
  def nextStep(): Long =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = iterate(myCurrent)
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(half: Int): LongTableStepper[I] = new LongTableStepper[I](maxLength, table, iterate, extract, i0, half)
}

