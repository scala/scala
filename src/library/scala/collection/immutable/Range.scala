/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Range.scala 18987 2009-10-08 18:31:44Z odersky $

package scala.collection.immutable

/** <p>
 *    The <code>Range</code> class represents integer values in range
 *    <code>[start;end)</code> with non-zero step value <code>step</code>.
 *    It's a special case of an indexed sequence.
 *    For example:
 *  </p><pre>
 *     <b>val</b> r1 = 0 until 10
 *     <b>val</b> r2 = r1.start until r1.end by r1.step + 1
 *     println(r2.length) // = 5
 *  </pre>
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.5
 */
@serializable @SerialVersionUID(7618862778670199309L)
class Range(val start: Int, val end: Int, val step: Int) extends IndexedSeq[Int] {

  require(step != 0)

  protected def copy(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Create a new range with the `start` and `end` values of this range and
   *  a new <code>step</code>.
   */
  def by(step: Int): Range = copy(start, end, step)

  def isInclusive = false

  override def foreach[@specialized(Unit) U](f: Int => U) {
    if (fullLength > 0) {
      val last = this.last
      var i = start
      while (i != last) {
        f(i)
        i += step
      }
      f(i)
    }
  }

  override def last: Int = if (step == 1 || step == -1) {
    end - step
  } else {
    val size = end.toLong - start.toLong
    val inclusiveLast = (size / step.toLong * step.toLong + start.toLong).toInt
    if (size % step == 0) inclusiveLast - step else inclusiveLast
  }

  def length: Int = fullLength.toInt

  protected def fullLength: Long = if (end > start == step > 0 && start != end)
    ((last.toLong - start.toLong) / step.toLong + 1)
  else
    0

  final override def isEmpty = length == 0

  @inline
  final def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  // take and drop have to be tolerant of large values without overflowing
  private def locationAfterN(n: Int) = if (n > 0) {
    if (step > 0)
      ((start.toLong + step.toLong * n.toLong) min last.toLong).toInt
    else
      ((start.toLong + step.toLong * n.toLong) max last.toLong).toInt
  } else {
    start
  }

  final override def take(n: Int): Range = if (n > 0 && length > 0) {
    Range(start, locationAfterN(n - 1), step).inclusive
  } else {
    Range(start, start, step)
  }

  final override def drop(n: Int): Range =
    copy(locationAfterN(n), end, step)

  final override def init: Range =
    take(length - 1)

  final override def slice(from: Int, until: Int): Range =
    drop(from).take(until - from)

  private def skip(p: Int => Boolean): Int = {
    var s = start
    if (length > 0) {
      val last = this.last
      while ((if (step > 0) s <= last else s >= last) && p(s))
        s += step
    }
    s
  }

  final override def takeWhile(p: Int => Boolean): Range = Range(start, skip(p), step)
  final override def dropWhile(p: Int => Boolean): Range = copy(skip(p), end, step)

  final override def span(p: Int => Boolean): (Range, Range) = {
    val split = skip(p)
    (Range(start, split, step), copy(split, end, step))
  }

  final override def splitAt(n: Int) = (take(n), drop(n))

  final override def takeRight(n: Int): Range = drop(length - n)

  final override def dropRight(n: Int): Range = take(length - n)

  final override def reverse: Range = if (length > 0) new Range.Inclusive(last, start, -step) else this

  /** Make range inclusive.
   */
  def inclusive = new Range.Inclusive(start, end, step)

  final def contains(x: Int): Boolean = if (length > 0) {
    if (step > 0) start <= x && x <= last && (x - start) % step == 0
    else start >= x && x >= last && (start - x) % step == 0
  } else {
    false
  }

  override def equals(other: Any) = other match {
    case x: Range =>
      length == x.length &&
      (length == 0 ||
       start == x.start &&
       (length == 1 || step == x.step))
    case _ =>
      super.equals(other)
  }

  /* eliminated, so as to not break the hashcode/equals contract
  override def hashCode = start + limit + step
  */

  override def toString() = {
    val endStr = if (length > Range.MAX_PRINT) ", ... )" else ")"
    take(Range.MAX_PRINT).mkString("Range(", ", ", endStr)
  }
}

object Range {
  private[immutable] val MAX_PRINT = 512  // some arbitrary value

  class Inclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    override def isInclusive = true
    override protected def copy(start: Int, end: Int, step: Int): Range = new Inclusive(start, end, step)
    override def last: Int = if (step == 1 || step == -1)
      end
    else
      ((end.toLong - start.toLong) / step.toLong * step.toLong + start.toLong).toInt
    protected override def fullLength: Long = if (end > start == step > 0 || start == end)
      ((last.toLong - start.toLong) / step.toLong + 1)
    else
      0
  }

  /** Make a range from `start` until `end` (exclusive) with given step value.
   * @note step != 0
   */
  def apply(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Make an range from `start` to `end` inclusive with step value 1.
   */
  def apply(start: Int, end: Int): Range with ByOne = new Range(start, end, 1) with ByOne

  /** Make an inclusive range from start to end with given step value.
   * @note step != 0
   */
  def inclusive(start: Int, end: Int, step: Int): Range.Inclusive = new Inclusive(start, end, step)

  /** Make an inclusive range from start to end with step value 1.
   */
  def inclusive(start: Int, end: Int): Range.Inclusive with ByOne = new Inclusive(start, end, 1) with ByOne

  trait ByOne extends Range {
    override final def foreach[@specialized(Unit) U](f: Int => U) {
      if (length > 0) {
        val last = this.last
        var i = start
        while (i != last) {
          f(i)
          i += 1
        }
        f(i)
      }
    }
  }

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
    implicit val bigDecAsIntegral = scala.Numeric.BigDecimalAsIfIntegral

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
    implicit val bigDecAsIntegral = scala.Numeric.BigDecimalAsIfIntegral
    implicit val doubleAsIntegral = scala.Numeric.DoubleAsIfIntegral
    def toBD(x: Double): BigDecimal = scala.BigDecimal valueOf x

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
