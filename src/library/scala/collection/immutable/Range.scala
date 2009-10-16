/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
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
class Range(val start: Int, val end: Int, val step: Int) extends Vector[Int] {

  require(step != 0)

  protected def copy(start: Int, end: Int, step: Int): Range = new Range(start, end, step)

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(step: Int): Range = copy(start, end, step)

  def isInclusive = false

  protected def limit = end

  override def foreach[U](f: Int => U) {
    var i = start
    while (if (step > 0) i < limit else i > limit) {
      f(i)
      i += step
    }
  }

  lazy val length: Int = {
    def plen(start: Int, limit: Int, step: Int) =
      if (limit <= start) 0 else (limit - start - 1) / step + 1
    if (step > 0) plen(start, limit, step)
    else plen(limit, start, -step)
  }

  final override def isEmpty =
    if (step > 0) start >= limit else start <= limit

  @inline
  final def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  final override def take(n: Int): Range = {
    val limit1 = start + step * (n max 0)
    if (step > 0) Range(start, limit1 min limit, step)
    else Range(start, limit1 max limit, step)
  }

  final override def drop(n: Int): Range =
    copy(start + step * (n max 0), end, step)

  final override def init: Range =
    take(length - 1)

  final override def slice(from: Int, until: Int): Range =
    drop(from).take(until - from)

  private def skip(p: Int => Boolean): Int = {
    var s = start
    while ((if (step > 0) s < limit else s > limit) && p(s)) s += step
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

  final override def reverse: Range = new Range.Inclusive(last, start, -step)

  def inclusive = new Range.Inclusive(start, end, step)

  def contains(x: Int): Boolean =
    if (step > 0) start <= x && x < limit && (x - start) % step == 0
    else start >= x && x > limit && (start - x) % step == 0

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
    override protected val limit = end + Math.signum(step)
    override protected def copy(start: Int, end: Int, step: Int): Range = new Inclusive(start, end, step)
  }

  def apply(start: Int, end: Int, step: Int): Range = new Range(start, end, step)
  def apply(start: Int, end: Int): Range with ByOne = new Range(start, end, 1) with ByOne
  def inclusive(start: Int, end: Int, step: Int): Range.Inclusive = new Inclusive(start, end, step)
  def inclusive(start: Int, end: Int): Range.Inclusive with ByOne = new Inclusive(start, end, 1) with ByOne

  trait ByOne extends Range {
    override final def foreach[U](f: Int => U) {
      var i = start
      val l = limit
      while (i < l) {
        f(i)
        i += 1
      }
    }
  }

  // BigInt and Long are straightforward generic ranges.
  object BigInt {
    def apply(start: BigInt, end: BigInt, step: BigInt) = GenericRange(start, end, step)
    def inclusive(start: BigInt, end: BigInt, step: BigInt) = GenericRange.inclusive(start, end, step)
  }

  object Long {
    def apply(start: Long, end: Long, step: Long) = GenericRange(start, end, step)
    def inclusive(start: Long, end: Long, step: Long) = GenericRange.inclusive(start, end, step)
  }

  // BigDecimal uses an alternative implementation of Numeric in which
  // it pretends to be Integral[T] instead of Fractional[T].  See Numeric for
  // details.  The intention is for it to throw an exception anytime
  // imprecision or surprises might result from anything, although this may
  // not yet be fully implemented.
  object BigDecimal {
    implicit val bigDecAsIntegral = scala.Numeric.BigDecimalAsIfIntegral

    def apply(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      GenericRange(start, end, step)
    def inclusive(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      GenericRange.inclusive(start, end, step)
  }

  // Double re-uses BigDecimal's range.
  object Double {
    def apply(start: Double, end: Double, step: Double) = scala.BigDecimal(start) until end by step
    def inclusive(start: Double, end: Double, step: Double) = scala.BigDecimal(start) to end by step
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
    def apply(start: Int, end: Int, step: Int) = GenericRange(start, end, step)
    def inclusive(start: Int, end: Int, step: Int) = GenericRange.inclusive(start, end, step)
  }
}
