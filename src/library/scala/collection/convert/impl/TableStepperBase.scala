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

private[collection] abstract class TableStepperBase[A, I >: Null <: AnyRef, Sub >: Null, Semi <: Sub with TableStepperBase[A, I, _, _]](
  protected var maxLength: Int, protected val table: Array[I], protected var i0: Int, protected val iN: Int
)
extends EfficientSubstep {
  protected var myCurrent: I = if (i0 < iN) table(i0) else null

  protected final def findNextCurrent(): Boolean = {
    while (i0 < iN && { myCurrent = table(i0); myCurrent eq null }) i0 += 1
    myCurrent ne null
  }

  protected def semiclone(half: Int): Semi

  def characteristics: Int = 0

  def estimateSize: Long = if (!hasNext) { maxLength = 0; 0 } else maxLength

  def hasNext: Boolean = (myCurrent ne null) || findNextCurrent()

  def substep(): Sub = {
    if (iN-1 > i0 && maxLength > 0) {
      val half = (i0 + iN) >>> 1
      val ans = semiclone(half)
      ans.myCurrent = myCurrent
      myCurrent = table(half)
      var inLeft = if (ans.myCurrent ne null) 1 else 0
      var inRight = if (myCurrent ne null) 1 else 0
      if (iN - i0 < 32) {
        var i = i0+1
        while (i < half && (table(i) ne null)) inLeft += 1
        i = half+1
        while (i < iN && (table(i) ne null)) inRight += 1
      }
      maxLength -= inLeft
      ans.maxLength -= inRight
      i0 = half
      ans
    }
    else null
  }
}
