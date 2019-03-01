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

import scala.collection.convert.impl.AccumulatorFactoryShape
import scala.collection.mutable


/**
 * Base class to share code between the [[AnyAccumulator]] class (for reference types) and the manual
 * specializations [[IntAccumulator]], [[LongAccumulator]] and [[DoubleAccumulator]].
 */
trait Accumulator[@specialized(Double, Int, Long) A, +CC[_], +C]
  extends mutable.Iterable[A]
    with mutable.Builder[A, C] {
  private[convert] var index: Int = 0
  private[convert] var hIndex: Int = 0
  private[convert] var totalSize: Long = 0L
  private[convert] def cumulative(i: Int): Long

  private[convert] def nextBlockSize: Int = {
    if (totalSize < 32) 16
    else if (totalSize <= Int.MaxValue) {
      val bit = 64 - java.lang.Long.numberOfLeadingZeros(totalSize)
      1 << (bit - (bit >> 2))
    }
    else 1 << 24
  }

  final override def size: Int = if (longSize < Int.MaxValue) longSize.toInt else Int.MaxValue /* not sure.. default implementation just overflows */

  final override def knownSize: Int = size

  /** Size of the accumulated collection, as a `Long` */
  final def longSize: Long = totalSize

  /** Remove all accumulated elements from this accumulator. */
  def clear(): Unit = {
    index = 0
    hIndex = 0
    totalSize = 0L
  }

  private[convert] def seekSlot(ix: Long): Long = {
    var lo = -1
    var hi = hIndex
    while (lo + 1 < hi) {
      val m = (lo + hi) >>> 1    // Shift allows division-as-unsigned, prevents overflow
      if (cumulative(m) > ix) hi = m
      else lo = m
    }
    (hi.toLong << 32) | (if (hi==0) ix else ix - cumulative(hi-1)).toInt
  }
}

object Accumulator {
  implicit def toFactory[A, C](sa: Accumulator.type)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): collection.Factory[A, C] = canAccumulate.factory
}
