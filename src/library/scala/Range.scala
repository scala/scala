/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

import collection.immutable.Vector
import collection.generic.VectorView

/** <p>
 *    <code>GenericRange</code> is a generified version of the
 *    <code>Range</code> class which works with arbitrary types.
 *    It must be supplied with Integral and Ordering implementations
 *    of the range type.
 *
 *    Factories for likely types include Range.BigInt and Range.Long.
 *    Range.Int exists for completeness, but the Int-based scala.Range
 *    should be more performant.
 *  </p><pre>
 *     <b>val</b> r1 = new Range(0, 100, 1)
 *     <b>val</b> veryBig = Math.MAX_INT.toLong + 1
 *     <b>val</b> r2 = Range.Long(veryBig, veryBig + 100, 1)
 *     assert(r1 sameElements r2.map(_ - veryBig))
 *  </pre>
 *
 *  @author  Paul Phillips
 *  @version 2.8
 */
abstract class GenericRange[T]
  (val start: T, val end: T, val step: T)
  (implicit num: Integral[T], ord: Ordering[T])
extends VectorView[T, Vector[T]] with RangeToString[T] {
  import num._
  import ord._

  // this lets us pretend all ranges are exclusive
  val isInclusive: Boolean
  private val trueEnd = if (isInclusive) end + one else end

  // todo? - we could lift the length restriction by implementing a range as a sequence of
  // subranges and limiting the subranges to MAX_INT.  There's no other way around it because
  // the generics we inherit assume integer-based indexing (as well they should.)
  require(step !== zero)
  require(genericLength <= fromInt(Math.MAX_INT), "Implementation restricts ranges to Math.MAX_INT elements.")

  protected def underlying = Vector.empty[T]

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(step: T): GenericRange[T] =
    if (isInclusive) GenericRange.inclusive(start, end, step)
    else GenericRange(start, end, step)

  override def foreach[U](f: T => U) {
    var i = start
    if (step > zero) {
      while (i < trueEnd) {
        f(i)
        i = i + step
      }
    } else {
      while (i > trueEnd) {
        f(i)
        i = i + step
      }
    }
  }

  lazy val genericLength: T = {
    def plen(start: T, end: T, step: T) =
      if (trueEnd <= start) zero
      else (trueEnd - start - one) / step + one

    if (step > zero) plen(start, trueEnd, step)
    else plen(trueEnd, start, -step)
  }
  lazy val length: Int = toInt(genericLength)

  // Since apply(Int) already exists, we are not allowed apply(T) since
  // they erase to the same thing.
  def apply(idx: Int): T = applyAt(fromInt(idx))
  def applyAt(idx: T): T = {
    if (idx < zero || idx >= genericLength) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  override def contains(_x: Any): Boolean = {
    // XXX - can we avoid this cast and still have a contains method?
    val x =
      try   { _x.asInstanceOf[T] }
      catch { case _: ClassCastException => return false }

    if (step > zero) start <= x && x < trueEnd
    else start >= x && x > trueEnd
  }
}

private[scala] trait RangeToString[T] extends VectorView[T, Vector[T]] {
  // The default toString() tries to print every element and will exhaust memory
  // if the Range is unduly large.  This interacts poorly with the REPL.
  override def toString() = {
    val MAX_PRINT = 512  // some arbitrary value
    val str = (this take MAX_PRINT).toString

    if (length > MAX_PRINT) str.replaceAll("""\)$""", ", ...)")
    else str
  }
}


object GenericRange {
  import Numeric._
  import Ordering._

  class Inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T], ord: Ordering[T])
  extends GenericRange(start, end, step)
  {
    val isInclusive = true
    def exclusive: Exclusive[T] = new Exclusive(start, end, step)
  }
  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T], ord: Ordering[T])
  extends GenericRange(start, end, step)
  {
    val isInclusive = false
    def inclusive: Inclusive[T] = new Inclusive(start, end, step)
  }

  def apply[T](start: T, end: T, step: T)(implicit num: Integral[T], ord: Ordering[T]) =
    new Exclusive(start, end, step)

  def inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T], ord: Ordering[T]) =
    new Inclusive(start, end, step)
}


/** <p>
 *    The <code>Range</code> class represents integer values in range
 *    <code>[start;end)</code> with non-zero step value <code>step</code>.
 *    Sort of acts like a sequence also (supports length and contains).
 *    For example:
 *  </p><pre>
 *     <b>val</b> r1 = 0 until 10
 *     <b>val</b> r2 = r1.start until r1.end by r1.step + 1
 *     println(r2.length) // = 5
 *  </pre>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
class Range(val start: Int, val end: Int, val step: Int)
extends VectorView[Int, Vector[Int]] with RangeToString[Int]
{
  require(step != 0)

  protected def underlying = Vector.empty[Int]

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(step: Int): Range = new Range(start, end, step)

  final override def foreach[U](f: Int => U) {
    var i = start
    if (step > 0) {
      while (i < end) {
        f(i)
        i += step
      }
    } else {
      while (i > end) {
        f(i)
        i += step
      }
    }
  }

  lazy val length: Int = {
    def plen(start: Int, end: Int, step: Int) =
      if (end <= start) 0 else (end - start - 1) / step + 1
    if (step > 0) plen(start, end, step)
    else plen(end, start, -step)
  }

  @inline
  final def apply(idx: Int): Int = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    start + idx * step
  }

  def contains(x: Int): Boolean =
    if (step > 0) start <= x && x < end
    else start >= x && x > end

  def inclusive = Range.inclusive(start, end, step)
}

object Range {
  /** @deprecated use Range.inclusive instead */
  final class Inclusive(start: Int, end0: Int, step: Int)
      extends Range(start, if (step > 0) end0 + 1 else end0 - 1, step) { self =>
    override def by(step: Int): Range = new Inclusive(start, end0, step)
  }

  def apply(start: Int, end: Int, step: Int) =
    new Range(start, end, step)

  def inclusive(start: Int, end: Int, step: Int): Range =
    new Range.Inclusive(start, end, step)

  object BigInt {
    def apply(start: BigInt, end: BigInt, step: BigInt) = GenericRange(start, end, step)
    def inclusive(start: BigInt, end: BigInt, step: BigInt) = GenericRange.inclusive(start, end, step)
  }
  object Long {
    def apply(start: Long, end: Long, step: Long) = GenericRange(start, end, step)
    def inclusive(start: Long, end: Long, step: Long) = GenericRange.inclusive(start, end, step)
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
