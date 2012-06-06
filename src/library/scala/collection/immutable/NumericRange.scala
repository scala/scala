/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package immutable

import mutable.{ Builder, ListBuffer }
import generic._

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
 *     val r1 = new Range(0, 100, 1)
 *     val veryBig = Int.MaxValue.toLong + 1
 *     val r2 = Range.Long(veryBig, veryBig + 100, 1)
 *     assert(r1 sameElements r2.map(_ - veryBig))
 *  }}}
 *
 *  TODO: Now the specialization exists there is no clear reason to have
 *  separate classes for Range/NumericRange.  Investigate and consolidate.
 *
 *  @author  Paul Phillips
 *  @version 2.8
 *  @define Coll `NumericRange`
 *  @define coll numeric range
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class NumericRange[T]
  (val start: T, val end: T, val step: T, val isInclusive: Boolean)
  (implicit num: Integral[T])
extends AbstractSeq[T] with IndexedSeq[T] with Serializable {
  /** Note that NumericRange must be invariant so that constructs
   *  such as "1L to 10 by 5" do not infer the range type as AnyVal.
   */
  import num._

  // See comment in Range for why this must be lazy.
  private lazy val numRangeElements: Int =
    NumericRange.count(start, end, step, isInclusive)

  override def length  = numRangeElements
  override def isEmpty = length == 0
  override lazy val last: T =
    if (length == 0) Nil.last
    else locationAfterN(length - 1)

  /** Create a new range with the start and end values of this range and
   *  a new `step`.
   */
  def by(newStep: T): NumericRange[T] = copy(start, end, newStep)

  /** Create a copy of this range.
   */
  def copy(start: T, end: T, step: T): NumericRange[T]

  override def foreach[U](f: T => U) {
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

  // Counts how many elements from the start meet the given test.
  private def skipCount(p: T => Boolean): Int = {
    var current = start
    var counted = 0

    while (counted < length && p(current)) {
      counted += 1
      current += step
    }
    counted
  }
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

  final override def take(n: Int): NumericRange[T] = (
    if (n <= 0 || length == 0) newEmptyRange(start)
    else if (n >= length) this
    else new NumericRange.Inclusive(start, locationAfterN(n - 1), step)
  )

  final override def drop(n: Int): NumericRange[T] = (
    if (n <= 0 || length == 0) this
    else if (n >= length) newEmptyRange(end)
    else copy(locationAfterN(n), end, step)
  )

  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    else locationAfterN(idx)
  }
  
  import NumericRange.defaultOrdering
  
  override def min[T1 >: T](implicit ord: Ordering[T1]): T =
    if (ord eq defaultOrdering(num)) {
      if (num.signum(step) > 0) start
      else last
    } else super.min(ord)
  
  override def max[T1 >: T](implicit ord: Ordering[T1]): T = 
    if (ord eq defaultOrdering(num)) {
      if (num.signum(step) > 0) last
      else start
    } else super.max(ord)
  
  // Motivated by the desire for Double ranges with BigDecimal precision,
  // we need some way to map a Range and get another Range.  This can't be
  // done in any fully general way because Ranges are not arbitrary
  // sequences but step-valued, so we have a custom method only we can call
  // which we promise to use responsibly.
  //
  // The point of it all is that
  //
  //   0.0 to 1.0 by 0.1
  //
  // should result in
  //
  //   NumericRange[Double](0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  //
  // and not
  //
  //   NumericRange[Double](0.0, 0.1, 0.2, 0.30000000000000004, 0.4, 0.5, 0.6000000000000001, 0.7000000000000001, 0.8, 0.9)
  //
  // or perhaps more importantly,
  //
  //   (0.1 to 0.3 by 0.1 contains 0.3) == true
  //
  private[immutable] def mapRange[A](fm: T => A)(implicit unum: Integral[A]): NumericRange[A] = {
    val self = this

    // XXX This may be incomplete.
    new NumericRange[A](fm(start), fm(end), fm(step), isInclusive) {
      def copy(start: A, end: A, step: A): NumericRange[A] =
        if (isInclusive) NumericRange.inclusive(start, end, step)
        else NumericRange(start, end, step)

      private lazy val underlyingRange: NumericRange[T] = self
      override def foreach[U](f: A => U) { underlyingRange foreach (x => f(fm(x))) }
      override def isEmpty = underlyingRange.isEmpty
      override def apply(idx: Int): A = fm(underlyingRange(idx))
      override def containsTyped(el: A) = underlyingRange exists (x => fm(x) == el)
    }
  }

  // a well-typed contains method.
  def containsTyped(x: T): Boolean =
    isWithinBoundaries(x) && (((x - start) % step) == zero)

  override def contains(x: Any): Boolean =
    try containsTyped(x.asInstanceOf[T])
    catch { case _: ClassCastException => false }

  final override def sum[B >: T](implicit num: Numeric[B]): B = {
    import num.Ops
    if (isEmpty) this.num fromInt 0
    else if (numRangeElements == 1) head
    else ((this.num fromInt numRangeElements) * (head + last) / (this.num fromInt 2))
  }

  override lazy val hashCode = super.hashCode()
  override def equals(other: Any) = other match {
    case x: NumericRange[_] =>
      (x canEqual this) && (length == x.length) && (
        (length == 0) ||                      // all empty sequences are equal
        (start == x.start && last == x.last)  // same length and same endpoints implies equality
      )
    case _ =>
      super.equals(other)
  }

  override def toString() = {
    val endStr = if (length > Range.MAX_PRINT) ", ... )" else ")"
    take(Range.MAX_PRINT).mkString("NumericRange(", ", ", endStr)
  }
}

/** A companion object for numeric ranges.
 */
object NumericRange {
  
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
      val diff      = num.minus(end, start)
      val jumps     = num.toLong(num.quot(diff, step))
      val remainder = num.rem(diff, step)
      val longCount = jumps + (
        if (!isInclusive && zero == remainder) 0 else 1
      )

      /** The edge cases keep coming.  Since e.g.
       *    Long.MaxValue + 1 == Long.MinValue
       *  we do some more improbable seeming checks lest
       *  overflow turn up as an empty range.
       */
      // The second condition contradicts an empty result.
      val isOverflow = longCount == 0 && num.lt(num.plus(start, step), end) == upward

      if (longCount > scala.Int.MaxValue || longCount < 0L || isOverflow) {
        val word  = if (isInclusive) "to" else "until"
        val descr = List(start, word, end, "by", step) mkString " "

        throw new IllegalArgumentException(descr + ": seqs cannot contain more than Int.MaxValue elements.")
      }
      longCount.toInt
    }
  }

  class Inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
  extends NumericRange(start, end, step, true) {
    def copy(start: T, end: T, step: T): Inclusive[T] =
      NumericRange.inclusive(start, end, step)

    def exclusive: Exclusive[T] = NumericRange(start, end, step)
  }

  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
  extends NumericRange(start, end, step, false) {
    def copy(start: T, end: T, step: T): Exclusive[T] =
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
    Numeric.FloatAsIfIntegral -> Ordering.Float,
    Numeric.DoubleAsIfIntegral -> Ordering.Double,
    Numeric.BigDecimalAsIfIntegral -> Ordering.BigDecimal
  )
  
}

