/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

// TODO: Now the specialization exists there is no clear reason to have
// separate classes for Range/NumericRange.  Investigate and consolidate.

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

      override def toString = {
        def simpleOf(x: Any): String = x.getClass.getName.split("\\.").last
        val stepped = simpleOf(underlyingRange.step)
        s"${super.toString} (using $underlyingRange of $stepped)"
      }
    }
  }

  // a well-typed contains method.
  def containsTyped(x: T): Boolean =
    isWithinBoundaries(x) && (((x - start) % step) == zero)

  override def contains[A1 >: T](x: A1): Boolean =
    try containsTyped(x.asInstanceOf[T])
    catch { case _: ClassCastException => false }

  final override def sum[B >: T](implicit num: Numeric[B]): B = {
    if (isEmpty) num.zero
    else if (numRangeElements == 1) head
    else {
      // If there is no overflow, use arithmetic series formula
      //   a + ... (n terms total) ... + b = n*(a+b)/2
      if ((num eq scala.math.Numeric.IntIsIntegral)||
          (num eq scala.math.Numeric.ShortIsIntegral)||
          (num eq scala.math.Numeric.ByteIsIntegral)||
          (num eq scala.math.Numeric.CharIsIntegral)) {
        // We can do math with no overflow in a Long--easy
        val exact = (numRangeElements * ((num toLong head) + (num toInt last))) / 2
        num fromInt exact.toInt
      }
      else if (num eq scala.math.Numeric.LongIsIntegral) {
        // Uh-oh, might be overflow, so we have to divide before we overflow.
        // Either numRangeElements or (head + last) must be even, so divide the even one before multiplying
        val a = head.toLong
        val b = last.toLong
        val ans =
          if ((numRangeElements & 1) == 0) (numRangeElements / 2) * (a + b)
          else numRangeElements * {
            // Sum is even, but we might overflow it, so divide in pieces and add back remainder
            val ha = a/2
            val hb = b/2
            ha + hb + ((a - 2*ha) + (b - 2*hb)) / 2
          }
        ans.asInstanceOf[B]
      }
      else if ((num eq scala.math.Numeric.FloatAsIfIntegral) ||
               (num eq scala.math.Numeric.DoubleAsIfIntegral)) {
        // Try to compute sum with reasonable accuracy, avoiding over/underflow
        val numAsIntegral = num.asInstanceOf[Integral[B]]
        import numAsIntegral._
        val a = math.abs(head.toDouble)
        val b = math.abs(last.toDouble)
        val two = num fromInt 2
        val nre = num fromInt numRangeElements
        if (a > 1e38 || b > 1e38) nre * ((head / two) + (last / two))  // Compute in parts to avoid Infinity if possible
        else (nre / two) * (head + last)    // Don't need to worry about infinity; this will be more accurate and avoid underflow
      }
      else if ((num eq scala.math.Numeric.BigIntIsIntegral) ||
               (num eq scala.math.Numeric.BigDecimalIsFractional)) {
        // No overflow, so we can use arithmetic series formula directly
        // (not going to worry about running out of memory)
        val numAsIntegral = num.asInstanceOf[Integral[B]]
        import numAsIntegral._
        ((num fromInt numRangeElements) * (head + last)) / (num fromInt 2)
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

  override def toString = {
    val empty = if (isEmpty) "empty " else ""
    val preposition = if (isInclusive) "to" else "until"
    val stepped = if (step == 1) "" else s" by $step"
    s"${empty}NumericRange $start $preposition $end$stepped"
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
      val one = num.one
      val limit = num.fromInt(Int.MaxValue)
      def check(t: T): T =
        if (num.gt(t, limit)) throw new IllegalArgumentException("More than Int.MaxValue elements.")
        else t
      // If the range crosses zero, it might overflow when subtracted
      val startside = num.signum(start)
      val endside = num.signum(end)
      num.toInt{
        if (startside*endside >= 0) {
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
          //   * there to the end
          val negone = num.fromInt(-1)
          val startlim  = if (posStep) negone else one
          val startdiff = num.minus(startlim, start)
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

