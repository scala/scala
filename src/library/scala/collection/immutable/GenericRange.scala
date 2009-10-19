/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: GenericRange.scala 18987 2009-10-08 18:31:44Z odersky $

package scala.collection.immutable

import annotation.experimental

import collection.VectorView
import util.control.Exception.catching
import util.Hashable

/** <p>
 *    <code>GenericRange</code> is a generified version of the
 *    <code>Range</code> class which works with arbitrary types.
 *    It must be supplied with an Integral implementation of the
 *    range type.
 *
 *    Factories for likely types include Range.BigInt, Range.Long,
 *    and Range.BigDecimal.  Range.Int exists for completeness, but
 *    the Int-based scala.Range should be more performant.
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
@experimental
abstract class GenericRange[+T]
  (val start: T, val end: T, val step: T, val isInclusive: Boolean)
  (implicit num: Integral[T])
extends VectorView[T, collection.immutable.Vector[T]]
{
  import num._

  // todo? - we could lift the length restriction by implementing a range as a sequence of
  // subranges and limiting the subranges to MAX_INT.  There's no other way around it because
  // the generics we inherit assume integer-based indexing (as well they should.)
  require(!(step equiv zero))
  require(genericLength <= fromInt(Math.MAX_INT), "Implementation restricts ranges to Math.MAX_INT elements.")

  // inclusive/exclusiveness captured this way because we do not have any
  // concept of a "unit", we can't just add an epsilon to an exclusive
  // endpoint to make it inclusive (as can be done with the int-based Range.)
  protected def limitTest[U >: T](x: U)(implicit unum: Integral[U]) =
    !isEmpty && isInclusive && unum.equiv(x, end)

  protected def underlying = collection.immutable.Vector.empty[T]

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by[U >: T](newStep: U)(implicit unum: Integral[U]): GenericRange[U] =
    copy(start, end, newStep)

  /** Create a copy of this range.
   */
  def copy[U >: T](start: U, end: U, step: U)(implicit unum: Integral[U]): GenericRange[U]

  override def foreach[U](f: T => U) {
    var i = start
    if (step > zero) {
      while (i < end) {
        f(i)
        i = i + step
      }
    } else {
      while (i > end) {
        f(i)
        i = i + step
      }
    }
    if (limitTest(i)) f(i)
  }

  def genericLength: T = {
    def lim = if (limitTest(end)) one else zero

    if ((start < end && step < zero) || (start > end && step > zero)) zero
    else if (equiv(start, end)) lim
    else {
      val (steps, left) = (end - start) /% step
      val last = if (!equiv(left, zero) || isInclusive) one else zero

      steps + last
    }
  }

  def length: Int = toInt(genericLength)
  final override def isEmpty =
    if (step > zero)
      if (isInclusive) end < start
      else end <= start
    else
      if (isInclusive) end > start
      else end >= start

  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    else start + (fromInt(idx) * step)
  }

  // a well-typed contains method.
  def containsTyped[U >: T](x: U)(implicit unum: Integral[U]): Boolean = {
    import unum._
    def divides(d: U, by: U) = equiv(d % by, zero)

    limitTest(x) || (
      if (step > zero)
        (start <= x) && (x < end) && divides(x - start, step)
      else
        (start >= x) && (x > end) && divides(start - x, step)
    )
  }

  // The contains situation makes for some interesting code.
  // I am not aware of any way to avoid a cast somewhere, because
  // contains must take an Any.
  override def contains(x: Any): Boolean =
    try {
      // if we don't verify that x == typedX, then a range
      // of e.g. Longs will appear to contain an Int because
      // the cast will perform the conversion.  (As of this writing
      // it is anticipated that in scala 2.8, 5L != 5 although
      // this is not yet implemented.)
      val typedX = x.asInstanceOf[T]
      containsTyped(typedX) && (x == typedX)
    }
    catch { case _: ClassCastException => super.contains(x) }

  override lazy val hashCode = super.hashCode()
  override def equals(other: Any) = other match {
    case x: GenericRange[_] => (length == x.length) && (length match {
      case 0  => true
      case 1  => x.start == start
      case n  => x.start == start && x.step == step
    })
    case _  => super.equals(other)
  }
  override def toString() = {
    val endStr = if (length > Range.MAX_PRINT) ", ... )" else ")"
    take(Range.MAX_PRINT).mkString("GenericRange(", ", ", endStr)
  }
}


object GenericRange
{
  class Inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
  extends GenericRange(start, end, step, true) {
    def copy[U >: T](start: U, end: U, step: U)(implicit unum: Integral[U]): Inclusive[U] =
      GenericRange.inclusive(start, end, step)

    def exclusive: Exclusive[T] = GenericRange(start, end, step)
  }

  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
  extends GenericRange(start, end, step, false) {
    def copy[U >: T](start: U, end: U, step: U)(implicit unum: Integral[U]): Exclusive[U] =
      GenericRange(start, end, step)

    def inclusive: Inclusive[T] = GenericRange.inclusive(start, end, step)
  }

  def apply[T](start: T, end: T, step: T)(implicit num: Integral[T]): Exclusive[T] =
    new Exclusive(start, end, step)
  def inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T]): Inclusive[T] =
    new Inclusive(start, end, step)
}

