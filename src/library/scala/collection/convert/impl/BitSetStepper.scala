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
import scala.collection.{BitSetOps, IntStepper, Stepper}


private[collection] final class BitSetStepper(
  private var underlying: BitSetOps[_], 
  private var cache0: Long, private var cache1: Long, 
  _i0: Int, _iN: Int,
  private var cacheIndex: Int
)
extends InOrderStepperBase[IntStepper, BitSetStepper](_i0, _iN)
with IntStepper {
  import BitSetOps.{WordLength, LogWL}

  // When `found` is set, `i0` is an element that exists
  protected var found: Boolean = false

  @annotation.tailrec
  protected def findNext(): Boolean =
    if (i0 >= iN) false
    else {
      val ix = i0 >> LogWL
      if (ix == cacheIndex || ix == cacheIndex+1) {
        val i = scanLong(if (ix == cacheIndex) cache0 else cache1, i0 & (WordLength - 1))
        if (i >= 0) {
          i0 = (i0 & ~(WordLength - 1)) | i
          found = (i0 < iN)
          found
        }
        else {
          i0 = (i0 & ~(WordLength - 1)) + WordLength
          findNext()
        }
      }
      else if (underlying eq null) { 
        i0 = iN
        found = false
        found
      }
      else {
        cacheIndex = ix
        cache0 = underlying.word(cacheIndex)
        cache1 = if ((iN - 1) >> LogWL == ix) -1L else underlying.word(cacheIndex+1)
        findNext()
      }
    }

  def semiclone(half: Int): BitSetStepper =
    if (underlying == null) {
      val ans = new BitSetStepper(null, cache0, cache1, i0, half, cacheIndex)
      ans.found = found
      i0 = half
      found = false
      ans
    }
    else {
      // Set up new stepper
      val ixNewN = (half - 1) >> LogWL
      val ans =
        new BitSetStepper(if (ixNewN <= cacheIndex + 1) null else underlying, cache0, cache1, i0, half, cacheIndex)
      if (found) ans.found = true

      // Advance old stepper to breakpoint
      val ixOld0 = half       >> LogWL
      if (ixOld0 > cacheIndex + 1) {
        cache0 = underlying.word(ixOld0)
        cache1 = if (((iN - 1) >> LogWL) == ixOld0) -1L else underlying.word(ixOld0+1)
        cacheIndex = ixOld0
        i0 = half
        found = false
      }

      // Return new stepper
      ans
    }

  @annotation.tailrec
  private[this] def scanLong(bits: Long, from: Int): Int =
    if (from >= WordLength) -1
    else if ((bits & (1L << from)) != 0) from
    else scanLong(bits, from + 1)

  def nextStep(): Int =
    if (found || findNext()) { 
      found = false
      val ans = i0
      i0 += 1
      ans
    }
    else Stepper.throwNSEE()
}

private[collection] object BitSetStepper {
  def from(bs: scala.collection.BitSetOps[_]): IntStepper with EfficientSplit =
    new BitSetStepper(
      if (bs.nwords <= 2) null else bs,
      if (bs.nwords <= 0) -1L else bs.word(0),
      if (bs.nwords <= 1) -1L else bs.word(1),
      0,
      bs.nwords * BitSetOps.WordLength,
      0
    )
}
