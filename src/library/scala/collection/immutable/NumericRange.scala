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

package scala.collection.immutable

import scala.collection.Stepper.EfficientSplit
import scala.collection.{AbstractIterator, AnyStepper, IterableFactoryDefaults, Iterator, Stepper, StepperShape}

/** `NumericRange` is a more generic version of the
  *  `Range` class which works with arbitrary types.
  *  It must be supplied with an `Integral` implementation of the
  *  range type.
  *
  *  Factories for likely types include `Range.BigInt`, `Range.Long`,
  *  and `Range.BigDecimal`.  `Range.Int` exists for completeness, but
  *  the `Int`-based `scala.Range` should be more performant.
  *
  *  {{{
  *     val r1 = Range(0, 100, 1)
  *     val veryBig = Int.MaxValue.toLong + 1
  *     val r2 = Range.Long(veryBig, veryBig + 100, 1)
  *     assert(r1 sameElements r2.map(_ - veryBig))
  *  }}}
  *
  *  @define Coll `NumericRange`
  *  @define coll numeric range
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(3L)
sealed class NumericRange[T](
  val start: T,
  val end: T,
  val step: T,
  val isInclusive: Boolean
)(implicit
  num: Integral[T]
)
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]]
    with StrictOptimizedSeqOps[T, IndexedSeq, IndexedSeq[T]]
    with IterableFactoryDefaults[T, IndexedSeq]
    with Serializable { self =>

  override def iterator: Iterator[T] = new NumericRange.NumericRangeIterator(this, num)

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit = {
    import scala.collection.convert._
    import impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntNumericRangeStepper   (this.asInstanceOf[NumericRange[Int]],    0, length)
      case StepperShape.LongShape   => new LongNumericRangeStepper  (this.asInstanceOf[NumericRange[Long]],   0, length)
      case _         => shape.parUnbox(new AnyNumericRangeStepper[T](this, 0, length).asInstanceOf[AnyStepper[T] with EfficientSplit])
    }
    s.asInstanceOf[S with EfficientSplit]
  }


  /** Note that NumericRange must be invariant so that constructs
    *  such as "1L to 10 by 5" do not infer the range type as AnyVal.
    */
  import num._

  // See comment in Range for why this must be lazy.
  override lazy val length: Int     = NumericRange.count(start, end, step, isInclusive)
  override lazy val isEmpty: Boolean = (
    (num.gt(start, end) && num.gt(step, num.zero))
      || (num.lt(start, end) && num.lt(step, num.zero))
      || (num.equiv(start, end) && !isInclusive)
    )
  override def last: T =
    if (isEmpty) Nil.head
    else locationAfterN(length - 1)
  override def init: NumericRange[T] =
    if (isEmpty) Nil.init
    else new NumericRange(start, end - step, step, isInclusive)

  override def head: T = if (isEmpty) Nil.head else start
  override def tail: NumericRange[T] =
    if (isEmpty) Nil.tail
    else if(isInclusive) new NumericRange.Inclusive(start + step, end, step)
    else new NumericRange.Exclusive(start + step, end, step)

  /** Create a new range with the start and end values of this range and
    *  a new `step`.
    */
  def by(newStep: T): NumericRange[T] = copy(start, end, newStep)


  /** Create a copy of this range.
    */
  def copy(start: T, end: T, step: T): NumericRange[T] =
    new NumericRange(start, end, step, isInclusive)

  @throws[IndexOutOfBoundsException]
  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${length - 1})")
    else locationAfterN(idx)
  }

  override def foreach[@specialized(Specializable.Unit) U](f: T => U): Unit = {
    var count = 0
    var current = start
    while (count < length) {
      f(current)
      current += step
      count += 1
    }
  }

  // TODO: these private methods are straight copies from Range, duplicated
  // to guard against any (most likely illusory) performance drop.  They should
  // be eliminated one way or another.

  // Tests whether a number is within the endpoints, without testing
  // whether it is a member of the sequence (i.e. when step > 1.)
  private def isWithinBoundaries(elem: T) = !isEmpty && (
    (step > zero && start <= elem && elem <= last ) ||
      (step < zero &&  last <= elem && elem <= start)
    )
  // Methods like apply throw exceptions on invalid n, but methods like take/drop
  // are forgiving: therefore the checks are with the methods.
  private def locationAfterN(n: Int): T = start + (step * fromInt(n))

  // When one drops everything.  Can't ever have unchecked operations
  // like "end + 1" or "end - 1" because ranges involving Int.{ MinValue, MaxValue }
  // will overflow.  This creates an exclusive range where start == end
  // based on the given value.
  private def newEmptyRange(value: T) = NumericRange(value, value, step)

  override def take(n: Int): NumericRange[T] = {
    if (n <= 0 || isEmpty) newEmptyRange(start)
    else if (n >= length) this
    else new NumericRange.Inclusive(start, locationAfterN(n - 1), step)
  }

  override def drop(n: Int): NumericRange[T] = {
    if (n <= 0 || isEmpty) this
    else if (n >= length) newEmptyRange(end)
    else copy(locationAfterN(n), end, step)
  }

  override def splitAt(n: Int): (NumericRange[T], NumericRange[T]) = (take(n), drop(n))

  override def reverse: NumericRange[T] =
    if (isEmpty) this
    else {
      val newStep = -step
      if (num.sign(newStep) == num.sign(step)) {
        throw new ArithmeticException("number type is unsigned, and .reverse requires a negative step")
      } else new NumericRange.Inclusive(last, start, newStep)
    }

  import NumericRange.defaultOrdering

  override def min[T1 >: T](implicit ord: Ordering[T1]): T =
  // We can take the fast path:
  // - If the Integral of this NumericRange is also the requested Ordering
  //   (Integral <: Ordering). This can happen for custom Integral types.
  // - The Ordering is the default Ordering of a well-known Integral type.
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) {
      if (num.sign(step) > zero) head
      else last
    } else super.min(ord)

  override def max[T1 >: T](implicit ord: Ordering[T1]): T =
  // See comment for fast path in min().
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) {
      if (num.sign(step) > zero) last
      else head
    } else super.max(ord)

  // a well-typed contains method.
  def containsTyped(x: T): Boolean =
    isWithinBoundaries(x) && (((x - start) % step) == zero)

  override def contains[A1 >: T](x: A1): Boolean =
    try containsTyped(x.asInstanceOf[T])
    catch { case _: ClassCastException => false }

  override def sum[B >: T](implicit num: Numeric[B]): B = {
    if (isEmpty) num.zero
    else if (size == 1) head
    else {
      // If there is no overflow, use arithmetic series formula
      //   a + ... (n terms total) ... + b = n*(a+b)/2
      if ((num eq scala.math.Numeric.IntIsIntegral)||
        (num eq scala.math.Numeric.ShortIsIntegral)||
        (num eq scala.math.Numeric.ByteIsIntegral)||
        (num eq scala.math.Numeric.CharIsIntegral)) {
        // We can do math with no overflow in a Long--easy
        val exact = (size * ((num toLong head) + (num toInt last))) / 2
        num fromInt exact.toInt
      }
      else if (num eq scala.math.Numeric.LongIsIntegral) {
        // Uh-oh, might be overflow, so we have to divide before we overflow.
        // Either numRangeElements or (head + last) must be even, so divide the even one before multiplying
        val a = head.toLong
        val b = last.toLong
        val ans =
          if ((size & 1) == 0) (size / 2) * (a + b)
          else size * {
            // Sum is even, but we might overflow it, so divide in pieces and add back remainder
            val ha = a/2
            val hb = b/2
            ha + hb + ((a - 2*ha) + (b - 2*hb)) / 2
          }
        ans.asInstanceOf[B]
      }
      else if ((num eq scala.math.Numeric.BigIntIsIntegral) ||
        (num eq scala.math.Numeric.BigDecimalAsIfIntegral)) {
        // No overflow, so we can use arithmetic series formula directly
        // (not going to worry about running out of memory)
        val numAsIntegral = num.asInstanceOf[Integral[B]]
        import numAsIntegral._
        ((num fromInt size) * (head + last)) / (num fromInt 2)
      }
      else {
        // User provided custom Numeric, so we cannot rely on arithmetic series formula (e.g. won't work on something like Z_6)
        if (isEmpty) num.zero
        else {
          var acc = num.zero
          var i = head
          var idx = 0
          while(idx < length) {
            acc = num.plus(acc, i)
            i = i + step
            idx = idx + 1
          }
          acc
        }
      }
    }
  }

  override lazy val hashCode: Int = super.hashCode()
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  override def equals(other: Any): Boolean = other match {
    case x: NumericRange[_] =>
      (x canEqual this) && (length == x.length) && (
        (isEmpty) ||                            // all empty sequences are equal
          (start == x.start && last == x.last)  // same length and same endpoints implies equality
        )
    case _ =>
      super.equals(other)
  }

  override def toString: String = {
    val empty = if (isEmpty) "empty " else ""
    val preposition = if (isInclusive) "to" else "until"
    val stepped = if (step == 1) "" else s" by $step"
    s"${empty}NumericRange $start $preposition $end$stepped"
  }

  override protected[this] def className = "NumericRange"
}

/** A companion object for numeric ranges.
  *  @define Coll `NumericRange`
  *  @define coll numeric range
  */
object NumericRange {
  private def bigDecimalCheckUnderflow[T](start: T, end: T, step: T)(implicit num: Integral[T]): Unit = {
    def FAIL(boundary: T, step: T): Unit = {
      val msg = boundary match {
        case bd: BigDecimal => s"Precision ${bd.mc.getPrecision}"
        case _              => "Precision"
      }
      throw new IllegalArgumentException(
        s"$msg inadequate to represent steps of size $step near $boundary"
      )
    }
    if (num.minus(num.plus(start, step), start) != step) FAIL(start, step)
    if (num.minus(end, num.minus(end, step))    != step) FAIL(end,   step)
  }

  /** Calculates the number of elements in a range given start, end, step, and
    *  whether or not it is inclusive.  Throws an exception if step == 0 or
    *  the number of elements exceeds the maximum Int.
    */
  def count[T](start: T, end: T, step: T, isInclusive: Boolean)(implicit num: Integral[T]): Int = {
    val zero    = num.zero
    val upward  = num.lt(start, end)
    val posStep = num.gt(step, zero)

    if (step == zero) throw new IllegalArgumentException("step cannot be 0.")
    else if (start == end) if (isInclusive) 1 else 0
    else if (upward != posStep) 0
    else {
      /* We have to be frightfully paranoid about running out of range.
       * We also can't assume that the numbers will fit in a Long.
       * We will assume that if a > 0, -a can be represented, and if
       * a < 0, -a+1 can be represented.  We also assume that if we
       * can't fit in Int, we can represent 2*Int.MaxValue+3 (at least).
       * And we assume that numbers wrap rather than cap when they overflow.
       */
      // Check whether we can short-circuit by deferring to Int range.
      val startint = num.toInt(start)
      if (start == num.fromInt(startint)) {
        val endint = num.toInt(end)
        if (end == num.fromInt(endint)) {
          val stepint = num.toInt(step)
          if (step == num.fromInt(stepint)) {
            return {
              if (isInclusive) Range.inclusive(startint, endint, stepint).length
              else             Range          (startint, endint, stepint).length
            }
          }
        }
      }
      // If we reach this point, deferring to Int failed.
      // Numbers may be big.
      if (num.isInstanceOf[Numeric.BigDecimalAsIfIntegral]) {
        bigDecimalCheckUnderflow(start, end, step)  // Throw exception if math is inaccurate (including no progress at all)
      }
      val one = num.one
      val limit = num.fromInt(Int.MaxValue)
      def check(t: T): T =
        if (num.gt(t, limit)) throw new IllegalArgumentException("More than Int.MaxValue elements.")
        else t
      // If the range crosses zero, it might overflow when subtracted
      val startside = num.sign(start)
      val endside = num.sign(end)
      num.toInt{
        if (num.gteq(num.times(startside, endside), zero)) {
          // We're sure we can subtract these numbers.
          // Note that we do not use .rem because of different conventions for Long and BigInt
          val diff = num.minus(end, start)
          val quotient = check(num.quot(diff, step))
          val remainder = num.minus(diff, num.times(quotient, step))
          if (!isInclusive && zero == remainder) quotient else check(num.plus(quotient, one))
        }
        else {
          // We might not even be able to subtract these numbers.
          // Jump in three pieces:
          //   * start to -1 or 1, whichever is closer (waypointA)
          //   * one step, which will take us at least to 0 (ends at waypointB)
          //   * (except with really small numbers)
          //   * there to the end
          val negone = num.fromInt(-1)
          val startlim  = if (posStep) negone else one
          //Use start value if the start value is closer to zero than startlim
          //   * e.g. .5 is closer to zero than 1 and -.5 is closer to zero than -1
          val startdiff = {
            if ((posStep && num.lt(startlim, start)) || (!posStep && num.gt(startlim, start)))
              start
            else
              num.minus(startlim, start)
          }
          val startq    = check(num.quot(startdiff, step))
          val waypointA = if (startq == zero) start else num.plus(start, num.times(startq, step))
          val waypointB = num.plus(waypointA, step)
          check {
            if (num.lt(waypointB, end) != upward) {
              // No last piece
              if (isInclusive && waypointB == end) num.plus(startq, num.fromInt(2))
              else num.plus(startq, one)
            }
            else {
              // There is a last piece
              val enddiff = num.minus(end,waypointB)
              val endq    = check(num.quot(enddiff, step))
              val last    = if (endq == zero) waypointB else num.plus(waypointB, num.times(endq, step))
              // Now we have to tally up all the pieces
              //   1 for the initial value
              //   startq steps to waypointA
              //   1 step to waypointB
              //   endq steps to the end (one less if !isInclusive and last==end)
              num.plus(startq, num.plus(endq, if (!isInclusive && last==end) one else num.fromInt(2)))
            }
          }
        }
      }
    }
  }

  @SerialVersionUID(3L)
  class Inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
    extends NumericRange(start, end, step, true) {
    override def copy(start: T, end: T, step: T): Inclusive[T] =
      NumericRange.inclusive(start, end, step)

    def exclusive: Exclusive[T] = NumericRange(start, end, step)
  }

  @SerialVersionUID(3L)
  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
    extends NumericRange(start, end, step, false) {
    override def copy(start: T, end: T, step: T): Exclusive[T] =
      NumericRange(start, end, step)

    def inclusive: Inclusive[T] = NumericRange.inclusive(start, end, step)
  }

  def apply[T](start: T, end: T, step: T)(implicit num: Integral[T]): Exclusive[T] =
    new Exclusive(start, end, step)
  def inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T]): Inclusive[T] =
    new Inclusive(start, end, step)

  private[collection] val defaultOrdering = Map[Numeric[_], Ordering[_]](
    Numeric.BigIntIsIntegral -> Ordering.BigInt,
    Numeric.IntIsIntegral -> Ordering.Int,
    Numeric.ShortIsIntegral -> Ordering.Short,
    Numeric.ByteIsIntegral -> Ordering.Byte,
    Numeric.CharIsIntegral -> Ordering.Char,
    Numeric.LongIsIntegral -> Ordering.Long,
    Numeric.BigDecimalAsIfIntegral -> Ordering.BigDecimal
  )

  @SerialVersionUID(3L)
  private final class NumericRangeIterator[T](self: NumericRange[T], num: Integral[T]) extends AbstractIterator[T] with Serializable {
    import num.mkNumericOps

    private[this] var _hasNext = !self.isEmpty
    private[this] var _next: T = self.start
    private[this] val lastElement: T = if (_hasNext) self.last else self.start
    override def knownSize: Int = if (_hasNext) num.toInt((lastElement - _next) / self.step) + 1 else 0
    def hasNext: Boolean = _hasNext
    def next(): T = {
      if (!_hasNext) Iterator.empty.next()
      val value = _next
      _hasNext = value != lastElement
      _next = num.plus(value, self.step)
      value
    }
  }
}
