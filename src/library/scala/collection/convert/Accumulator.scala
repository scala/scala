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

private[convert] class AccumulatorStepper[A](private val acc: AnyAccumulator[A]) extends AnyStepper[A] {
  import java.util.Spliterator._

  private var h = 0
  private var i = 0
  private var a = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N = acc.totalSize

  private def duplicateSelf(limit: Long): AccumulatorStepper[A] = {
    val ans = new AccumulatorStepper(acc)
    ans.h = h
    ans.i = i
    ans.a = a
    ans.n = n
    ans.N = limit
    ans
  }

  private def loadMore(): Unit = {
    h += 1
    if (h < acc.hIndex) { a = acc.history(h); n = acc.cumulative(h) - acc.cumulative(h-1) }
    else { a = acc.current; n = acc.index }
    i = 0
  }

  def characteristics = ORDERED | SIZED | SUBSIZED

  def estimateSize = N

  def hasNext = N > 0

  def next(): A =
    if (N <= 0) throw new NoSuchElementException("Next in empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i).asInstanceOf[A]
      i += 1
      N -= 1
      ans
    }

  // Overidden for efficiency
  override def tryStep(f: A => Unit): Boolean =
    if (N <= 0) false
    else {
      if (i >= n) loadMore()
      f(a(i).asInstanceOf[A])
      i += 1
      N -= 1
      true
    }

  // Overidden for efficiency
  override def tryAdvance(f: java.util.function.Consumer[_ >: A]): Boolean =
    if (N <= 0) false
    else {
      if (i >= n) loadMore()
      f.accept(a(i).asInstanceOf[A])
      i += 1
      N -= 1
      true
    }

  // Overridden for efficiency
  override def foreach(f: A => Unit): Unit = {
    while (N > 0) {
      if (i >= n) loadMore()
      val i0 = i
      if ((n-i) > N) n = i + N.toInt
      while (i < n) {
        f(a(i).asInstanceOf[A])
        i += 1
      }
      N -= (n - i0)
    }
  }

  // Overridden for efficiency
  override def forEachRemaining(f: java.util.function.Consumer[_ >: A]): Unit = {
    while (N > 0) {
      if (i >= n) loadMore()
      val i0 = i
      if ((n-i) > N) n = i + N.toInt
      while (i < n) {
        f.accept(a(i).asInstanceOf[A])
        i += 1
      }
      N -= (n - i0)
    }
  }

  def substep(): AnyStepper[A] =
    if (N <= 1) null
    else {
      val half = N >> 1
      val M = (if (h <= 0) 0L else acc.cumulative(h-1)) + i
      val R = M + half
      val ans = duplicateSelf(half)
      if (h < acc.hIndex) {
        val w = acc.seekSlot(R)
        h = (w >>> 32).toInt
        if (h < acc.hIndex) {
          a = acc.history(h)
          n = acc.cumulative(h) - (if (h > 0) acc.cumulative(h-1) else 0)
        }
        else {
          a = acc.current
          n = acc.index
        }
        i = (w & 0xFFFFFFFFL).toInt
      }
      else i += half.toInt
      N -= half
      ans
    }

  override def toString = s"$h $i ${a.mkString("{",",","}")} $n $N"
}
