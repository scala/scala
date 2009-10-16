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
abstract class GenericRange[T]
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
  protected def limitTest(x: T) = !isEmpty && isInclusive && equiv(x, end)
  protected def underlying = collection.immutable.Vector.empty[T]
  protected def divides(x: T, by: T) = equiv(x % by, zero)

  /** Create a new range with the start and end values of this range and
   *  a new <code>step</code>.
   */
  def by(newStep: T): GenericRange[T] = copy(start, end, newStep)

  /** Create a copy of this range.
   */
  def copy(start: T, end: T, step: T): GenericRange[T]

  /** Shift or multiply the entire range by some constant.
   */
  def -(shift: T) = this + negate(shift)
  def +(shift: T) = copy(this.start + shift, this.end + shift, step)
  def *(mult: T) = copy(this.start * mult, this.end * mult, step * mult)

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
      if (isInclusive) end > start
      else end >= start
    else
      if (isInclusive) start > end
      else start >= end

  // Since apply(Int) already exists, we are not allowed apply(T) since
  // they erase to the same thing.
  def apply(idx: Int): T = applyAt(fromInt(idx))
  def applyAt(idx: T): T = {
    if (idx < zero || idx >= genericLength) throw new IndexOutOfBoundsException(idx.toString)
    start + (idx * step)
  }

  // a well-typed contains method.
  def containsTyped(x: T): Boolean =
    limitTest(x) || (
      if (step > zero)
        (start <= x) && (x < end) && divides(x - start, step)
      else
        (start >= x) && (x > end) && divides(start - x, step)
    )

  // The contains situation makes for some interesting code.
  // I am not aware of any way to avoid a cast somewhere, because
  // contains must take an Any.
  override def contains(x: Any): Boolean =
    try containsTyped(x.asInstanceOf[T])
    catch { case _: ClassCastException => super.contains(x) }

  override def equals(other: Any) = other match {
    case x: GenericRange[_] => (genericLength == x.genericLength) && (genericLength match {
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
    def exclusive: Exclusive[T] = GenericRange(start, end, step)
    def copy(start: T, end: T, step: T): Inclusive[T] = GenericRange.inclusive(start, end, step)
  }

  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
  extends GenericRange(start, end, step, false) {
    def inclusive: Inclusive[T] = GenericRange.inclusive(start, end, step)
    def copy(start: T, end: T, step: T): Exclusive[T] = GenericRange(start, end, step)
  }

  def apply[T](start: T, end: T, step: T)(implicit num: Integral[T]): Exclusive[T] =
    new Exclusive(start, end, step)
  def inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T]): Inclusive[T] =
    new Inclusive(start, end, step)
}

