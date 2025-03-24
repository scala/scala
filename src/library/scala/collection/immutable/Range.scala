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

package scala
package collection.immutable

import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl.RangeStepper
import scala.collection.generic.CommonErrors
import scala.collection.{AbstractIterator, AnyStepper, IterableFactoryDefaults, Iterator, Stepper, StepperShape}
import scala.util.hashing.MurmurHash3

/** The `Range` class represents integer values in range
  *  ''[start;end)'' with non-zero step value `step`.
  *  It's a special case of an indexed sequence.
  *  For example:
  *
  *  {{{
  *     val r1 = 0 until 10
  *     val r2 = r1.start until r1.end by r1.step + 1
  *     println(r2.length) // = 5
  *  }}}
  *
  *  Ranges that contain more than `Int.MaxValue` elements can be created, but
  *  these overfull ranges have only limited capabilities. Any method that
  *  could require a collection of over `Int.MaxValue` length to be created, or
  *  could be asked to index beyond `Int.MaxValue` elements will throw an
  *  exception. Overfull ranges can safely be reduced in size by changing
  *  the step size (e.g. `by 3`) or taking/dropping elements. `contains`,
  *  `equals`, and access to the ends of the range (`head`, `last`, `tail`,
  *  `init`) are also permitted on overfull ranges.
  *
  *  @param start       the start of this range.
  *  @param end         the end of the range.  For exclusive ranges, e.g.
  *                     `Range(0,3)` or `(0 until 3)`, this is one
  *                     step past the last one in the range.  For inclusive
  *                     ranges, e.g. `Range.inclusive(0,3)` or `(0 to 3)`,
  *                     it may be in the range if it is not skipped by the step size.
  *                     To find the last element inside a non-empty range,
  *                     use `last` instead.
  *  @param step        the step for the range.
  *
  *  @define coll range
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  *  @define doesNotUseBuilders
  *    '''Note:''' this method does not use builders to construct a new range,
  *         and its complexity is O(1).
  */
@SerialVersionUID(3L)
sealed abstract class Range(
  val start: Int,
  val end: Int,
  val step: Int
)
  extends AbstractSeq[Int]
    with IndexedSeq[Int]
    with IndexedSeqOps[Int, IndexedSeq, IndexedSeq[Int]]
    with StrictOptimizedSeqOps[Int, IndexedSeq, IndexedSeq[Int]]
    with IterableFactoryDefaults[Int, IndexedSeq]
    with Serializable { range =>

  final override def iterator: Iterator[Int] = new RangeIterator(start, step, lastElement, isEmpty)

  override final def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = {
    val st = new RangeStepper(start, step, 0, length)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
  }

  private[this] def gap           = end.toLong - start.toLong
  private[this] def isExact       = gap % step == 0
  private[this] def hasStub       = isInclusive || !isExact
  private[this] def longLength    = gap / step + ( if (hasStub) 1 else 0 )

  def isInclusive: Boolean

  final override val isEmpty: Boolean = (
    (start > end && step > 0)
      || (start < end && step < 0)
      || (start == end && !isInclusive)
    )

  private[this] val numRangeElements: Int = {
    if (step == 0) throw new IllegalArgumentException("step cannot be 0.")
    else if (isEmpty) 0
    else {
      val len = longLength
      if (len > scala.Int.MaxValue) -1
      else len.toInt
    }
  }

  final def length = if (numRangeElements < 0) fail() else numRangeElements

  // This field has a sensible value only for non-empty ranges
  private[this] val lastElement = step match {
    case 1  => if (isInclusive) end else end-1
    case -1 => if (isInclusive) end else end+1
    case _  =>
      val remainder = (gap % step).toInt
      if (remainder != 0) end - remainder
      else if (isInclusive) end
      else end - step
  }

  /** The last element of this range.  This method will return the correct value
    *  even if there are too many elements to iterate over.
    */
  final override def last: Int =
    if (isEmpty) throw Range.emptyRangeError("last") else lastElement
  final override def head: Int =
    if (isEmpty) throw Range.emptyRangeError("head") else start

  /** Creates a new range containing all the elements of this range except the last one.
    *
    *  $doesNotUseBuilders
    *
    *  @return  a new range consisting of all the elements of this range except the last one.
    */
  final override def init: Range =
    if (isEmpty) throw Range.emptyRangeError("init") else dropRight(1)

  /** Creates a new range containing all the elements of this range except the first one.
    *
    *  $doesNotUseBuilders
    *
    *  @return  a new range consisting of all the elements of this range except the first one.
    */
  final override def tail: Range = {
    if (isEmpty) throw Range.emptyRangeError("tail")
    if (numRangeElements == 1) newEmptyRange(end)
    else if(isInclusive) new Range.Inclusive(start + step, end, step)
    else new Range.Exclusive(start + step, end, step)
  }

  override def map[B](f: Int => B): IndexedSeq[B] = {
    validateMaxLength()
    super.map(f)
  }

  final protected def copy(start: Int = start, end: Int = end, step: Int = step, isInclusive: Boolean = isInclusive): Range =
    if(isInclusive) new Range.Inclusive(start, end, step) else new Range.Exclusive(start, end, step)

  /** Create a new range with the `start` and `end` values of this range and
    *  a new `step`.
    *
    *  @return a new range with a different step
    */
  final def by(step: Int): Range = copy(start, end, step)

  // Check cannot be evaluated eagerly because we have a pattern where
  // ranges are constructed like: "x to y by z" The "x to y" piece
  // should not trigger an exception. So the calculation is delayed,
  // which means it will not fail fast for those cases where failing was
  // correct.
  private[this] def validateMaxLength(): Unit = {
    if (numRangeElements < 0)
      fail()
  }
  private[this] def description = "%d %s %d by %s".format(start, if (isInclusive) "to" else "until", end, step)
  private[this] def fail() = throw new IllegalArgumentException(description + ": seqs cannot contain more than Int.MaxValue elements.")

  @throws[IndexOutOfBoundsException]
  final def apply(idx: Int): Int = {
    validateMaxLength()
    if (idx < 0 || idx >= numRangeElements)
      throw CommonErrors.indexOutOfBounds(index = idx, max = numRangeElements - 1)
    else start + (step * idx)
  }

  /*@`inline`*/ final override def foreach[@specialized(Unit) U](f: Int => U): Unit = {
    // Implementation chosen on the basis of favorable microbenchmarks
    // Note--initialization catches step == 0 so we don't need to here
    if (!isEmpty) {
      var i = start
      while (true) {
        f(i)
        if (i == lastElement) return
        i += step
      }
    }
  }

  override final def indexOf[@specialized(Int) B >: Int](elem: B, from: Int = 0): Int =
    elem match {
      case i: Int =>
        val pos = posOf(i)
        if (pos >= from) pos else -1
      case _ => super.indexOf(elem, from)
    }

  override final def lastIndexOf[@specialized(Int) B >: Int](elem: B, end: Int = length - 1): Int =
    elem match {
      case i: Int =>
        val pos = posOf(i)
        if (pos <= end) pos else -1
      case _ => super.lastIndexOf(elem, end)
    }

  private[this] def posOf(i: Int): Int =
    if (contains(i)) (i - start) / step else -1

  override def sameElements[B >: Int](that: IterableOnce[B]): Boolean = that match {
    case other: Range =>
      (this.length : @annotation.switch) match {
        case 0 => other.isEmpty
        case 1 => other.length == 1 && this.start == other.start
        case n => other.length == n && (
          (this.start == other.start)
            && (this.step == other.step)
        )
      }
    case _ => super.sameElements(that)
  }

  /** Creates a new range containing the first `n` elements of this range.
    *
    *  @param n  the number of elements to take.
    *  @return   a new range consisting of `n` first elements.
    */
  final override def take(n: Int): Range =
    if (n <= 0 || isEmpty) newEmptyRange(start)
    else if (n >= numRangeElements && numRangeElements >= 0) this
    else {
      // May have more than Int.MaxValue elements in range (numRangeElements < 0)
      // but the logic is the same either way: take the first n
      new Range.Inclusive(start, locationAfterN(n - 1), step)
    }

  /** Creates a new range containing all the elements of this range except the first `n` elements.
    *
    *  @param n  the number of elements to drop.
    *  @return   a new range consisting of all the elements of this range except `n` first elements.
    */
  final override def drop(n: Int): Range =
    if (n <= 0 || isEmpty) this
    else if (n >= numRangeElements && numRangeElements >= 0) newEmptyRange(end)
    else {
      // May have more than Int.MaxValue elements (numRangeElements < 0)
      // but the logic is the same either way: go forwards n steps, keep the rest
      copy(locationAfterN(n), end, step)
    }

  /** Creates a new range consisting of the last `n` elements of the range.
    *
    *  $doesNotUseBuilders
    */
  final override def takeRight(n: Int): Range = {
    if (n <= 0) newEmptyRange(start)
    else if (numRangeElements >= 0) drop(numRangeElements - n)
    else {
      // Need to handle over-full range separately
      val y = last
      val x = y - step.toLong*(n-1)
      if ((step > 0 && x < start) || (step < 0 && x > start)) this
      else Range.inclusive(x.toInt, y, step)
    }
  }

  /** Creates a new range consisting of the initial `length - n` elements of the range.
    *
    *  $doesNotUseBuilders
    */
  final override def dropRight(n: Int): Range = {
    if (n <= 0) this
    else if (numRangeElements >= 0) take(numRangeElements - n)
    else {
      // Need to handle over-full range separately
      val y = last - step.toInt*n
      if ((step > 0 && y < start) || (step < 0 && y > start)) newEmptyRange(start)
      else Range.inclusive(start, y.toInt, step)
    }
  }

  // Advance from the start while we meet the given test
  private[this] def argTakeWhile(p: Int => Boolean): Long = {
    if (isEmpty) start
    else {
      var current = start
      val stop = last
      while (current != stop && p(current)) current += step
      if (current != stop || !p(current)) current
      else current.toLong + step
    }
  }

  final override def takeWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop==start) newEmptyRange(start)
    else {
      val x = (stop - step).toInt
      if (x == last) this
      else Range.inclusive(start, x, step)
    }
  }

  final override def dropWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop == start) this
    else {
      val x = (stop - step).toInt
      if (x == last) newEmptyRange(last)
      else Range.inclusive(x + step, last, step)
    }
  }

  final override def span(p: Int => Boolean): (Range, Range) = {
    val border = argTakeWhile(p)
    if (border == start) (newEmptyRange(start), this)
    else {
      val x = (border - step).toInt
      if (x == last) (this, newEmptyRange(last))
      else (Range.inclusive(start, x, step), Range.inclusive(x+step, last, step))
    }
  }

  /** Creates a new range containing the elements starting at `from` up to but not including `until`.
    *
    *  $doesNotUseBuilders
    *
    *  @param from  the element at which to start
    *  @param until  the element at which to end (not included in the range)
    *  @return   a new range consisting of a contiguous interval of values in the old range
    */
  final override def slice(from: Int, until: Int): Range =
    if (from <= 0) take(until)
    else if (until >= numRangeElements && numRangeElements >= 0) drop(from)
    else {
      val fromValue = locationAfterN(from)
      if (from >= until) newEmptyRange(fromValue)
      else Range.inclusive(fromValue, locationAfterN(until-1), step)
    }

  // Overridden only to refine the return type
  final override def splitAt(n: Int): (Range, Range) = (take(n), drop(n))

  // Methods like apply throw exceptions on invalid n, but methods like take/drop
  // are forgiving: therefore the checks are with the methods.
  private[this] def locationAfterN(n: Int) = start + (step * n)

  // When one drops everything.  Can't ever have unchecked operations
  // like "end + 1" or "end - 1" because ranges involving Int.{ MinValue, MaxValue }
  // will overflow.  This creates an exclusive range where start == end
  // based on the given value.
  private[this] def newEmptyRange(value: Int) = new Range.Exclusive(value, value, step)

  /** Returns the reverse of this range.
    */
  final override def reverse: Range =
    if (isEmpty) this
    else new Range.Inclusive(last, start, -step)

  /** Make range inclusive.
    */
  final def inclusive: Range =
    if (isInclusive) this
    else new Range.Inclusive(start, end, step)

  final def contains(x: Int): Boolean = {
    if (x == end && !isInclusive) false
    else if (step > 0) {
      if (x < start || x > end) false
      else (step == 1) || (Integer.remainderUnsigned(x - start, step) == 0)
    }
    else {
      if (x < end || x > start) false
      else (step == -1) || (Integer.remainderUnsigned(start - x, -step) == 0)
    }
  }
  /* Seq#contains has a type parameter so the optimised contains above doesn't override it */
  override final def contains[B >: Int](elem: B): Boolean = elem match {
    case i: Int => this.contains(i)
    case _      => super.contains(elem)
  }

  final override def sum[B >: Int](implicit num: Numeric[B]): Int = {
    if (num eq scala.math.Numeric.IntIsIntegral) {
      // this is normal integer range with usual addition. arithmetic series formula can be used
      if (isEmpty) 0
      else if (size == 1) head
      else ((size * (head.toLong + last)) / 2).toInt
    } else {
      // user provided custom Numeric, we cannot rely on arithmetic series formula
      if (isEmpty) num.toInt(num.zero)
      else {
        var acc = num.zero
        var i = head
        while (true) {
          acc = num.plus(acc, i)
          if (i == lastElement) return num.toInt(acc)
          i = i + step
        }
        0 // Never hit this--just to satisfy compiler since it doesn't know while(true) has type Nothing
      }
    }
  }

  final override def min[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) head
      else last
    } else if (Ordering.Int isReverseOf ord) {
      if (step > 0) last
      else head
    } else super.min(ord)

  final override def max[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) last
      else head
    } else if (Ordering.Int isReverseOf ord) {
      if (step > 0) head
      else last
    } else super.max(ord)

  override def tails: Iterator[Range] =
    new AbstractIterator[Range] {
      private[this] var i = 0
      override def hasNext = i <= Range.this.length
      override def next() = {
        if (hasNext) {
          val res = Range.this.drop(i)
          i += 1
          res
        } else {
          Iterator.empty.next()
        }
      }
    }

  override def inits: Iterator[Range] =
    new AbstractIterator[Range] {
      private[this] var i = 0
      override def hasNext = i <= Range.this.length
      override def next() = {
        if (hasNext) {
          val res = Range.this.dropRight(i)
          i += 1
          res
        } else {
          Iterator.empty.next()
        }
      }
    }
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  final override def equals(other: Any): Boolean = other match {
    case x: Range =>
      // Note: this must succeed for overfull ranges (length > Int.MaxValue)
      if (isEmpty) x.isEmpty                  // empty sequences are equal
      else                                    // this is non-empty...
        x.nonEmpty && start == x.start && {   // ...so other must contain something and have same start
          val l0 = last
          (l0 == x.last && (                    // And same end
            start == l0 || step == x.step       // And either the same step, or not take any steps
          ))
        }
    case _ =>
      super.equals(other)
  }

  final override def hashCode: Int =
    if(length >= 2) MurmurHash3.rangeHash(start, step, lastElement)
    else super.hashCode

  final override def toString: String = {
    val preposition = if (isInclusive) "to" else "until"
    val stepped = if (step == 1) "" else s" by $step"
    val prefix = if (isEmpty) "empty " else if (!isExact) "inexact " else ""
    s"${prefix}Range $start $preposition $end$stepped"
  }

  override protected[this] def className = "Range"

  override def distinct: Range = this

  override def grouped(size: Int): Iterator[Range] = {
    require(size >= 1, f"size=$size%d, but size must be positive")
    if (isEmpty) {
      Iterator.empty
    } else {
      val s = size
      new AbstractIterator[Range] {
        private[this] var i = 0
        override def hasNext = Range.this.length > i
        override def next() =
          if (hasNext) {
            val x = Range.this.slice(i, i + s)
            i += s
            x
          } else {
            Iterator.empty.next()
          }
      }
    }
  }

  override def sorted[B >: Int](implicit ord: Ordering[B]): IndexedSeq[Int] =
    if (ord eq Ordering.Int) {
      if (step > 0) {
        this
      } else {
        reverse
      }
    } else {
      super.sorted(ord)
    }
}

/**
  * Companion object for ranges.
  *  @define Coll `Range`
  *  @define coll range
  */
object Range {

  /** Counts the number of range elements.
    *  precondition:  step != 0
    *  If the size of the range exceeds Int.MaxValue, the
    *  result will be negative.
    */
  def count(start: Int, end: Int, step: Int, isInclusive: Boolean): Int = {
    if (step == 0)
      throw new IllegalArgumentException("step cannot be 0.")

    val isEmpty =
      if (start == end) !isInclusive
      else if (start < end) step < 0
      else step > 0

    if (isEmpty) 0
    else {
      // Counts with Longs so we can recognize too-large ranges.
      val gap: Long    = end.toLong - start.toLong
      val jumps: Long  = gap / step
      // Whether the size of this range is one larger than the
      // number of full-sized jumps.
      val hasStub      = isInclusive || (gap % step != 0)
      val result: Long = jumps + ( if (hasStub) 1 else 0 )

      if (result > scala.Int.MaxValue) -1
      else result.toInt
    }
  }
  def count(start: Int, end: Int, step: Int): Int =
    count(start, end, step, isInclusive = false)

  /** Make a range from `start` until `end` (exclusive) with given step value.
    * @note step != 0
    */
  def apply(start: Int, end: Int, step: Int): Range.Exclusive = new Range.Exclusive(start, end, step)

  /** Make a range from `start` until `end` (exclusive) with step value 1.
    */
  def apply(start: Int, end: Int): Range.Exclusive = new Range.Exclusive(start, end, 1)

  /** Make an inclusive range from `start` to `end` with given step value.
    * @note step != 0
    */
  def inclusive(start: Int, end: Int, step: Int): Range.Inclusive = new Range.Inclusive(start, end, step)

  /** Make an inclusive range from `start` to `end` with step value 1.
    */
  def inclusive(start: Int, end: Int): Range.Inclusive = new Range.Inclusive(start, end, 1)

  @SerialVersionUID(3L)
  final class Inclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    def isInclusive: Boolean = true
  }

  @SerialVersionUID(3L)
  final class Exclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    def isInclusive: Boolean = false
  }

  // BigInt and Long are straightforward generic ranges.
  object BigInt {
    def apply(start: BigInt, end: BigInt, step: BigInt): NumericRange.Exclusive[BigInt] = NumericRange(start, end, step)
    def inclusive(start: BigInt, end: BigInt, step: BigInt): NumericRange.Inclusive[BigInt] = NumericRange.inclusive(start, end, step)
  }

  object Long {
    def apply(start: Long, end: Long, step: Long): NumericRange.Exclusive[Long] = NumericRange(start, end, step)
    def inclusive(start: Long, end: Long, step: Long): NumericRange.Inclusive[Long] = NumericRange.inclusive(start, end, step)
  }

  // BigDecimal uses an alternative implementation of Numeric in which
  // it pretends to be Integral[T] instead of Fractional[T].  See Numeric for
  // details.  The intention is for it to throw an exception anytime
  // imprecision or surprises might result from anything, although this may
  // not yet be fully implemented.
  object BigDecimal {
    implicit val bigDecAsIntegral: Numeric.BigDecimalAsIfIntegral = Numeric.BigDecimalAsIfIntegral

    def apply(start: BigDecimal, end: BigDecimal, step: BigDecimal): NumericRange.Exclusive[BigDecimal] =
      NumericRange(start, end, step)
    def inclusive(start: BigDecimal, end: BigDecimal, step: BigDecimal): NumericRange.Inclusive[BigDecimal] =
      NumericRange.inclusive(start, end, step)
  }

  // As there is no appealing default step size for not-really-integral ranges,
  // we offer a partially constructed object.
  class Partial[T, U](private val f: T => U) extends AnyVal {
    def by(x: T): U = f(x)
    override def toString = "Range requires step"
  }

  // Illustrating genericity with Int Range, which should have the same behavior
  // as the original Range class.  However we leave the original Range
  // indefinitely, for performance and because the compiler seems to bootstrap
  // off it and won't do so with our parameterized version without modifications.
  object Int {
    def apply(start: Int, end: Int, step: Int): NumericRange.Exclusive[Int] = NumericRange(start, end, step)
    def inclusive(start: Int, end: Int, step: Int): NumericRange.Inclusive[Int] = NumericRange.inclusive(start, end, step)
  }

  private def emptyRangeError(what: String): Throwable =
    new NoSuchElementException(what + " on empty Range")
}

/**
  * @param lastElement The last element included in the Range
  * @param initiallyEmpty Whether the Range was initially empty or not
  */
@SerialVersionUID(3L)
private class RangeIterator(
  start: Int,
  step: Int,
  lastElement: Int,
  initiallyEmpty: Boolean
) extends AbstractIterator[Int] with Serializable {
  private[this] var _hasNext: Boolean = !initiallyEmpty
  private[this] var _next: Int = start
  override def knownSize: Int = if (_hasNext) (lastElement - _next) / step + 1 else 0
  def hasNext: Boolean = _hasNext
  @throws[NoSuchElementException]
  def next(): Int = {
    if (!_hasNext) Iterator.empty.next()
    val value = _next
    _hasNext = value != lastElement
    _next = value + step
    value
  }

  override def drop(n: Int): Iterator[Int] = {
    if (n > 0) {
      val longPos = _next.toLong + step * n
      if (step > 0) {
        _next = Math.min(lastElement, longPos).toInt
        _hasNext = longPos <= lastElement
      }
      else if (step < 0) {
        _next = Math.max(lastElement, longPos).toInt
        _hasNext = longPos >= lastElement
      }
    }
    this
  }
}
