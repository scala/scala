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

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   *  @note should never be called after `inclusive'.
   */
  def by(step: Int): Range = Range(start, end, step)

  override def foreach[U](f: Int => U) {
    var i = start
    while (if (step > 0) i < end else i > end) {
      f(i)
      i += step
    }
  }

  lazy val length: Int = {
    def plen(start: Int, end: Int, step: Int) =
      if (end <= start) 0 else (end - start - 1) / step + 1
    if (step > 0) plen(start, end, step)
    else plen(end, start, -step)
  }

  final override def isEmpty =
    if (step > 0) start >= end else start <= end

  @inline
  final def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  final override def init: Range =
    dropRight(1)

  final override def take(n: Int): Range = {
    val end1 = start + step * (n max 0)
    if (step > 0) Range(start, end1 min end, step)
    else Range(start, end1 max end, step)
  }

  final override def drop(n: Int): Range =
    if (step == 0) this
    else Range(start + step * n, end, step)

  final override def slice(from: Int, until: Int): Range =
    drop(from).take(until - from)

  final override def takeWhile(p: Int => Boolean): Range = {
    var s = start
    while (!isEmpty && p(s)) s += step
    Range(start, s, step)
  }

  final override def dropWhile(p: Int => Boolean): Range = {
    var s = start
    while (!isEmpty && p(s)) s += step
    Range(s, end, step)
  }

  final override def span(p: Int => Boolean): (Range, Range) = {
    var s = start
    while (!isEmpty && p(s)) s += step
    (Range(start, s, step), Range(s, end, step))
  }

  final override def splitAt(n: Int) = (take(n), drop(n))

  final override def takeRight(n: Int): Range = {
    val start1 = end - step * (n max 0)
    if (step > 0) Range(start1 max start, end, step)
    else Range(start1 min start, end, step)
  }

  final override def dropRight(n: Int): Range =
    Range(start, end - step * n, step)

  final override def reverse: Range =
    Range(end, start, -step)

  def contains(x: Int): Boolean =
    if (step > 0) start <= x && x < end && (x - start) % step == 0
    else start >= x && x > end && (start - x) % step == 0

  def inclusive = Range(start, end + Math.signum(step), step)

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
  override def hashCode = start + end + step
  */

  override def toString() = {
    val end = if (length > Range.MAX_PRINT) ", ... )" else ")"
    take(Range.MAX_PRINT).mkString("Range(", ", ", end)
  }
}

object Range {
  private val MAX_PRINT = 512  // some arbitrary value

  @deprecated("use Range.inclusive instead")
  final class Inclusive(start: Int, end0: Int, step: Int)
      extends Range(start, if (step > 0) end0 + 1 else end0 - 1, step) { self =>
    override def by(step: Int): Range = new Inclusive(start, end0, step)
  }

  // The standard / Int-specific Range.
  def apply(start: Int, end: Int, step: Int): Range =
    if (step == 1) new ByOne(start, end)
    else if (step > 0) new ByPosStep(start, end, step)
    else new ByNegStep(start, end, step)

  def inclusive(start: Int, end: Int, step: Int): Range =
    apply(start, end + Math.signum(step), step)

  class ByOne(start: Int, end: Int) extends Range(start, end, 1) {
    override final def foreach[U](f: Int => U) {
      var i = start
      while (i < end) {
        f(i)
        i += 1
      }
    }
  }

  class ByPosStep(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    override final def foreach[U](f: Int => U) {
      var i = start
      while (i < end) {
        f(i)
        i += step
      }
    }
  }

  class ByNegStep(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    override final def foreach[U](f: Int => U) {
      var i = start
      while (i > end) {
        f(i)
        i += step
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
