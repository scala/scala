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

private[convert] abstract class VectorStepperBase[Sub >: Null, Semi <: Sub](
  _i0: Int,
  _iN: Int,
  protected val displayN: Int,
  protected val trunk: Array[AnyRef]
)
extends IndexedStepperBase[Sub, Semi](_i0, _iN) {
  protected var index: Int = 32  // Force an advanceData on the first element
  protected var leaves: Array[AnyRef] = null
  protected var index1: Int = 32 // Force advanceData to defer to initTo on the first element
  protected var twigs: Array[AnyRef] = null

  protected final def advanceData(iX: Int): Unit = {
    index1 += 1
    if (index1 >= 32) initTo(iX)
    else {
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = 0
    }
  }
  protected final def initTo(iX: Int): Unit = displayN match {
    case 0 =>
      leaves = trunk
      index = iX
    case 1 =>
      twigs = trunk
      index1 = iX >>> 5
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = iX & 0x1F
    case _ =>
      var n = displayN
      var dataN = trunk
      while (n > 2) {
        dataN = dataN((iX >> (5*n)) & 0x1F).asInstanceOf[Array[AnyRef]]
        n -= 1
      }
      twigs = dataN((iX >>> 10) & 0x1F).asInstanceOf[Array[AnyRef]]
      index1 = (iX >> 5) & 0x1F
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = iX & 0x1F
  }
}

private[collection] class AnyVectorStepper[A](_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[AnyStepper[A], AnyVectorStepper[A]](_i0, _iN, _displayN, _trunk)
with AnyStepper[A] {
  def nextStep(): A = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[A]
  } else Stepper.throwNSEE()
  def semiclone(half: Int): AnyVectorStepper[A] = {
    val ans = new AnyVectorStepper[A](i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }
}

private[collection] class DoubleVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[DoubleStepper, DoubleVectorStepper](_i0, _iN, _displayN, _trunk)
with DoubleStepper {
  def nextStep(): Double = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Double]
  } else Stepper.throwNSEE()
  def semiclone(half: Int): DoubleVectorStepper = {
    val ans = new DoubleVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }    
}

private[collection] class IntVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[IntStepper, IntVectorStepper](_i0, _iN, _displayN, _trunk)
with IntStepper {
  def nextStep(): Int = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Int]
  } else Stepper.throwNSEE()
  def semiclone(half: Int): IntVectorStepper = {
    val ans = new IntVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }    
}

private[collection] class LongVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[LongStepper, LongVectorStepper](_i0, _iN, _displayN, _trunk)
with LongStepper {
  def nextStep(): Long = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Long]
  } else Stepper.throwNSEE()
  def semiclone(half: Int): LongVectorStepper = {
    val ans = new LongVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }    
}
