/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection.immutable

import scala.collection.parallel.immutable.ParRange

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
 *  these overfull ranges have only limited capabilities.  Any method that
 *  could require a collection of over `Int.MaxValue` length to be created, or
 *  could be asked to index beyond `Int.MaxValue` elements will throw an
 *  exception.  Overfull ranges can safely be reduced in size by changing
 *  the step size (e.g. `by 3`) or taking/dropping elements.  `contains`,
 *  `equals`, and access to the ends of the range (`head`, `last`, `tail`,
 *  `init`) are also permitted on overfull ranges.
 *
 *  @param start      the start of this range.
 *  @param end        the end of the range.  For exclusive ranges, e.g. 
 *                    `Range(0,3)` or `(0 until 3)`, this is one
 *                    step past the last one in the range.  For inclusive
 *                    ranges, e.g. `Range.inclusive(0,3)` or `(0 to 3)`,
 *                    it may be in the range if it is not skipped by the step size.
 *                    To find the last element inside a non-empty range,
                      use `last` instead.
 *  @param step       the step for the range.
 *
 *  @author Martin Odersky
 *  @author Paul Phillips
 *  @version 2.8
 *  @since   2.5
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#ranges "Scala's Collection Library overview"]]
 *  section on `Ranges` for more information.
 *
 *  @define coll range
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define doesNotUseBuilders
 *    '''Note:''' this method does not use builders to construct a new range,
 *         and its complexity is O(1).
 */
@SerialVersionUID(7618862778670199309L)
@deprecatedInheritance("The implementation details of Range makes inheriting from it unwise.", "2.11.0")
class Range(val start: Int, val end: Int, val step: Int)
extends scala.collection.AbstractSeq[Int]
   with IndexedSeq[Int]
   with scala.collection.CustomParallelizable[Int, ParRange]
   with Serializable
{
  override def par = new ParRange(this)

  private def gap           = end.toLong - start.toLong
  private def isExact       = gap % step == 0
  private def hasStub       = isInclusive || !isExact
  private def longLength    = gap / step + ( if (hasStub) 1 else 0 )

  // Check cannot be evaluated eagerly because we have a pattern where
  // ranges are constructed like: "x to y by z" The "x to y" piece
  // should not trigger an exception. So the calculation is delayed,
  // which means it will not fail fast for those cases where failing was
  // correct.
  override final val isEmpty = (
       (start > end && step > 0)
    || (start < end && step < 0)
    || (start == end && !isInclusive)
  )
  @deprecated("This method will be made private, use `length` instead.", "2.11")
  final val numRangeElements: Int = {
    if (step == 0) throw new IllegalArgumentException("step cannot be 0.")
    else if (isEmpty) 0
    else {
      val len = longLength
      if (len > scala.Int.MaxValue) -1
      else len.toInt
    }
  }
  @deprecated("This method will be made private, use `last` instead.", "2.11")
  final val lastElement = 
    if (isEmpty) start - step
    else step match {
      case 1  => if (isInclusive) end else end-1
      case -1 => if (isInclusive) end else end+1
      case _  =>
        val remainder = (gap % step).toInt
        if (remainder != 0) end - remainder
        else if (isInclusive) end
        else end - step
    }
    
  @deprecated("This method will be made private.", "2.11")
  final val terminalElement = lastElement + step

  /** The last element of this range.  This method will return the correct value
   *  even if there are too many elements to iterate over.
   */
  override def last = if (isEmpty) Nil.last else lastElement
  override def head = if (isEmpty) Nil.head else start

  override def min[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) head
      else last
    } else super.min(ord)

  override def max[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) last
      else head
    } else super.max(ord)

  protected def copy(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Create a new range with the `start` and `end` values of this range and
   *  a new `step`.
   *
   *  @return a new range with a different step
   */
  def by(step: Int): Range = copy(start, end, step)

  def isInclusive = false

  override def size = length
  override def length = if (numRangeElements < 0) fail() else numRangeElements

  private def description = "%d %s %d by %s".format(start, if (isInclusive) "to" else "until", end, step)
  private def fail() = throw new IllegalArgumentException(description + ": seqs cannot contain more than Int.MaxValue elements.")
  private def validateMaxLength() {
    if (numRangeElements < 0)
      fail()
  }

  final def apply(idx: Int): Int = {
    validateMaxLength()
    if (idx < 0 || idx >= numRangeElements) throw new IndexOutOfBoundsException(idx.toString)
    else start + (step * idx)
  }

  @inline final override def foreach[@specialized(Unit) U](f: Int => U) {
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

  /** Creates a new range containing the first `n` elements of this range.
   *
   *  $doesNotUseBuilders
   *
   *  @param n  the number of elements to take.
   *  @return   a new range consisting of `n` first elements.
   */
  final override def take(n: Int): Range = (
    if (n <= 0 || isEmpty) newEmptyRange(start)
    else if (n >= numRangeElements && numRangeElements >= 0) this
    else {
      // May have more than Int.MaxValue elements in range (numRangeElements < 0)
      // but the logic is the same either way: take the first n
      new Range.Inclusive(start, locationAfterN(n - 1), step)
    }
  )

  /** Creates a new range containing all the elements of this range except the first `n` elements.
   *
   *  $doesNotUseBuilders
   *
   *  @param n  the number of elements to drop.
   *  @return   a new range consisting of all the elements of this range except `n` first elements.
   */
  final override def drop(n: Int): Range = (
    if (n <= 0 || isEmpty) this
    else if (n >= numRangeElements && numRangeElements >= 0) newEmptyRange(end)
    else {
      // May have more than Int.MaxValue elements (numRangeElements < 0)
      // but the logic is the same either way: go forwards n steps, keep the rest
      copy(locationAfterN(n), end, step)
    }
  )

  /** Creates a new range containing all the elements of this range except the last one.
   *
   *  $doesNotUseBuilders
   *
   *  @return  a new range consisting of all the elements of this range except the last one.
   */
  final override def init: Range = {
    if (isEmpty)
      Nil.init

    dropRight(1)
  }

  /** Creates a new range containing all the elements of this range except the first one.
   *
   *  $doesNotUseBuilders
   *
   *  @return  a new range consisting of all the elements of this range except the first one.
   */
  final override def tail: Range = {
    if (isEmpty)
      Nil.tail

    drop(1)
  }

  // Advance from the start while we meet the given test
  private def argTakeWhile(p: Int => Boolean): Long = {
    if (isEmpty) start
    else {
      var current = start
      val stop = last
      while (current != stop && p(current)) current += step
      if (current != stop || !p(current)) current
      else current.toLong + step
    }
  }
  // Methods like apply throw exceptions on invalid n, but methods like take/drop
  // are forgiving: therefore the checks are with the methods.
  private def locationAfterN(n: Int) = start + (step * n)

  // When one drops everything.  Can't ever have unchecked operations
  // like "end + 1" or "end - 1" because ranges involving Int.{ MinValue, MaxValue }
  // will overflow.  This creates an exclusive range where start == end
  // based on the given value.
  private def newEmptyRange(value: Int) = new Range(value, value, step)

  final override def takeWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop==start) newEmptyRange(start)
    else {
      val x = (stop - step).toInt
      if (x == last) this
      else new Range.Inclusive(start, x, step)
    }
  }
  final override def dropWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop == start) this
    else {
      val x = (stop - step).toInt
      if (x == last) newEmptyRange(last)
      else new Range.Inclusive(x + step, last, step)
    }
  }
  final override def span(p: Int => Boolean): (Range, Range) = {
    val border = argTakeWhile(p)
    if (border == start) (newEmptyRange(start), this)
    else {
      val x = (border - step).toInt
      if (x == last) (this, newEmptyRange(last))
      else (new Range.Inclusive(start, x, step), new Range.Inclusive(x+step, last, step))
    }
  }

  /** Creates a pair of new ranges, first consisting of elements before `n`, and the second
   *  of elements after `n`.
   *
   *  $doesNotUseBuilders
   */
  final override def splitAt(n: Int) = (take(n), drop(n))

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
      else new Range.Inclusive(x.toInt, y, step)
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
      else new Range.Inclusive(start, y.toInt, step)
    }
  }

  /** Returns the reverse of this range.
   *
   *  $doesNotUseBuilders
   */
  final override def reverse: Range =
    if (isEmpty) this
    else new Range.Inclusive(last, start, -step)

  /** Make range inclusive.
   */
  def inclusive =
    if (isInclusive) this
    else new Range.Inclusive(start, end, step)

  final def contains(x: Int) = {
    if (x==end && !isInclusive) false
    else if (step > 0) {
      if (x < start || x > end) false
      else (step == 1) || (((x - start) % step) == 0)
    }
    else {
      if (x < end || x > start) false
      else (step == -1) || (((x - start) % step) == 0)
    }
  }

  final override def sum[B >: Int](implicit num: Numeric[B]): Int = {
    if (num eq scala.math.Numeric.IntIsIntegral) {
      // this is normal integer range with usual addition. arithmetic series formula can be used
      if (isEmpty) 0
      else if (numRangeElements == 1) head
      else ((numRangeElements * (head.toLong + last)) / 2).toInt
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

  override def toIterable = this

  override def toSeq = this

  override def equals(other: Any) = other match {
    case x: Range =>
      // Note: this must succeed for overfull ranges (length > Int.MaxValue)
      (x canEqual this) && {
        if (isEmpty) x.isEmpty                  // empty sequences are equal
        else                                    // this is non-empty...
          x.nonEmpty && start == x.start && {   // ...so other must contain something and have same start
            val l0 = last
            (l0 == x.last && (                    // And same end
              start == l0 || step == x.step       // And either the same step, or not take any steps
            ))
          }
      }
    case _ =>
      super.equals(other)
  }
  /** Note: hashCode can't be overridden without breaking Seq's
   *  equals contract.
   */

  override def toString() = {
    val endStr =
      if (numRangeElements > Range.MAX_PRINT || (!isEmpty && numRangeElements < 0)) ", ... )" else ")"
    take(Range.MAX_PRINT).mkString("Range(", ", ", endStr)
  }
}

/** A companion object for the `Range` class.
 */
object Range {
  private[immutable] val MAX_PRINT = 512  // some arbitrary value

  /** Counts the number of range elements.
   *  @pre  step != 0
   *  If the size of the range exceeds Int.MaxValue, the
   *  result will be negative.
   */
  def count(start: Int, end: Int, step: Int, isInclusive: Boolean): Int = {
    if (step == 0)
      throw new IllegalArgumentException("step cannot be 0.")

    val isEmpty = (
      if (start == end) !isInclusive
      else if (start < end) step < 0
      else step > 0
    )
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

  class Inclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
//    override def par = new ParRange(this)
    override def isInclusive = true
    override protected def copy(start: Int, end: Int, step: Int): Range = new Inclusive(start, end, step)
  }

  /** Make a range from `start` until `end` (exclusive) with given step value.
   * @note step != 0
   */
  def apply(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Make a range from `start` until `end` (exclusive) with step value 1.
   */
  def apply(start: Int, end: Int): Range = new Range(start, end, 1)

  /** Make an inclusive range from `start` to `end` with given step value.
   * @note step != 0
   */
  def inclusive(start: Int, end: Int, step: Int): Range.Inclusive = new Inclusive(start, end, step)

  /** Make an inclusive range from `start` to `end` with step value 1.
   */
  def inclusive(start: Int, end: Int): Range.Inclusive = new Inclusive(start, end, 1)

  // BigInt and Long are straightforward generic ranges.
  object BigInt {
    def apply(start: BigInt, end: BigInt, step: BigInt) = NumericRange(start, end, step)
    def inclusive(start: BigInt, end: BigInt, step: BigInt) = NumericRange.inclusive(start, end, step)
  }

  object Long {
    def apply(start: Long, end: Long, step: Long) = NumericRange(start, end, step)
    def inclusive(start: Long, end: Long, step: Long) = NumericRange.inclusive(start, end, step)
  }

  // BigDecimal uses an alternative implementation of Numeric in which
  // it pretends to be Integral[T] instead of Fractional[T].  See Numeric for
  // details.  The intention is for it to throw an exception anytime
  // imprecision or surprises might result from anything, although this may
  // not yet be fully implemented.
  object BigDecimal {
    implicit val bigDecAsIntegral = scala.math.Numeric.BigDecimalAsIfIntegral

    def apply(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      NumericRange(start, end, step)
    def inclusive(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      NumericRange.inclusive(start, end, step)
  }

  // Double works by using a BigDecimal under the hood for precise
  // stepping, but mapping the sequence values back to doubles with
  // .doubleValue.  This constructs the BigDecimals by way of the
  // String constructor (valueOf) instead of the Double one, which
  // is necessary to keep 0.3d at 0.3 as opposed to
  // 0.299999999999999988897769753748434595763683319091796875 or so.
  object Double {
    implicit val bigDecAsIntegral = scala.math.Numeric.BigDecimalAsIfIntegral
    implicit val doubleAsIntegral = scala.math.Numeric.DoubleAsIfIntegral
    def toBD(x: Double): BigDecimal = scala.math.BigDecimal valueOf x

    def apply(start: Double, end: Double, step: Double) =
      BigDecimal(toBD(start), toBD(end), toBD(step)) mapRange (_.doubleValue)

    def inclusive(start: Double, end: Double, step: Double) =
      BigDecimal.inclusive(toBD(start), toBD(end), toBD(step)) mapRange (_.doubleValue)
  }

  // As there is no appealing default step size for not-really-integral ranges,
  // we offer a partially constructed object.
  class Partial[T, U](f: T => U) {
    def by(x: T): U = f(x)
  }

  // Illustrating genericity with Int Range, which should have the same behavior
  // as the original Range class.  However we leave the original Range
  // indefinitely, for performance and because the compiler seems to bootstrap
  // off it and won't do so with our parameterized version without modifications.
  object Int {
    def apply(start: Int, end: Int, step: Int) = NumericRange(start, end, step)
    def inclusive(start: Int, end: Int, step: Int) = NumericRange.inclusive(start, end, step)
  }
}
