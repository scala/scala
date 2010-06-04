/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.immutable

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
 *  @param start      the start of this range.
 *  @param end        the exclusive end of the range.
 *  @param step       the step for the range.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.5
 *  @define Coll Range
 *  @define coll range
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define doesNotUseBuilders
 *    '''Note:''' this method does not use builders to construct a new range,
 *         and its complexity is O(1).
 */
@serializable @SerialVersionUID(7618862778670199309L)
class Range(val start: Int, val end: Int, val step: Int) extends IndexedSeq[Int] {

  require(step != 0)

  protected def copy(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Create a new range with the `start` and `end` values of this range and
   *  a new `step`.
   *
   *  @return a new range with a different step
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
  // warning! this is buggy, and gives wrong answers on boundary cases.
  // The known bugs are avoided by drop not calling it in those cases.
  // See ticket #3529.  It should be revised.
  private def locationAfterN(n: Int) = if (n > 0) {
    if (step > 0)
      ((start.toLong + step.toLong * n.toLong) min last.toLong).toInt
    else
      ((start.toLong + step.toLong * n.toLong) max last.toLong).toInt
  } else {
    start
  }

  /** Creates a new range containing the first `n` elements of this range.
   *
   *  $doesNotUseBuilders
   *
   *  @param n  the number of elements to take.
   *  @return   a new range consisting of `n` first elements.
   */
  final override def take(n: Int): Range =
    if (n > 0 && length > 0)
      Range(start, locationAfterN(n - 1), step).inclusive
    else
      Range(start, start, step)

  /** Creates a new range containing all the elements of this range except the first `n` elements.
   *
   *  $doesNotUseBuilders
   *
   *  @param n  the number of elements to drop.
   *  @return   a new range consisting of all the elements of this range except `n` first elements.
   */
  final override def drop(n: Int): Range =
    if (n >= length) copy(end + 1, end, step)
    else copy(locationAfterN(n), end, step)

  /** Creates a new range containing all the elements of this range except the last one.
   *
   *  $doesNotUseBuilders
   *
   *  @return  a new range consisting of all the elements of this range except the last one.
   */
  final override def init: Range =
    take(length - 1)

  /** Creates a new range contained in the specified slice of this range.
   *
   *  $doesNotUseBuilders
   *
   *  @param from   the start of the slice.
   *  @param until  the end of the slice.
   *  @return       a new range consisting of all the elements of this range contained in the specified slice.
   */
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

  /** Creates a pair of new ranges, first consisting of elements before `n`, and the second
   *  of elements after `n`.
   *
   *  $doesNotUseBuilders
   */
  final override def splitAt(n: Int) = (take(n), drop(n))

  /** Creates a new range consisting of the `length - n` last elements of the range.
   *
   *  $doesNotUseBuilders
   */
  final override def takeRight(n: Int): Range = drop(length - n)

  /** Creates a new range consisting of the initial `length - n` elements of the range.
   *
   *  $doesNotUseBuilders
   */
  final override def dropRight(n: Int): Range = take(length - n)

  /** Returns the reverse of this range.
   *
   *  $doesNotUseBuilders
   */
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

/** A companion object for the `Range` class.
 */
object Range {
  private[immutable] val MAX_PRINT = 512  // some arbitrary value

  /** Calculates the number of elements in a range given start, end, step, and
   *  whether or not it is inclusive.  Returns -1 if parameters are invalid.
   */
  def count(start: Int, end: Int, step: Int): Int = count(start, end, step, false)
  def count(start: Int, end: Int, step: Int, isInclusive: Boolean): Int = {
    def last =
      if (isInclusive && step < 0) end - 1
      else if (isInclusive && step > 0) end + 1
      else end

    if (step == 0) -1
    else if (start == end) { if (isInclusive) 1 else 0 }
    else if (end > start != step > 0) -1
    else if (step == 1 || step == -1) last - start
    else ((last - start - 1) / step) + 1
  }

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
