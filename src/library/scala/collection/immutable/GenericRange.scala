/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Range.scala 18987 2009-10-08 18:31:44Z odersky $

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
  (val start: T, val end: T, val step: T, val isInclusive: Boolean = false)
  (implicit num: Integral[T])
extends VectorView[T, collection.immutable.Vector[T]]
   with RangeToString[T] // !!! I think this does too little to be its trait --> see simplified impl ion Range
   with Hashable // !!! not needed because it inherits from Vector
{

  import num._

  // todo? - we could lift the length restriction by implementing a range as a sequence of
  // subranges and limiting the subranges to MAX_INT.  There's no other way around it because
  // the generics we inherit assume integer-based indexing (as well they should.)
  require(!(step equiv zero))
  require(genericLength <= fromInt(Math.MAX_INT), "Implementation restricts ranges to Math.MAX_INT elements.")

  // By adjusting end based on isInclusive, we can treat all ranges as exclusive.
  private lazy val trueEnd: T = if (isInclusive) end + step else end
  protected def underlying = collection.immutable.Vector.empty[T]

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
    def plen(s: T, e: T, stp: T) =
      if (e <= s) zero else ((e - s) / stp)

    if (step > zero) plen(start, trueEnd, step)
    else plen(trueEnd, start, -step)
  }
  lazy val length: Int = toInt(genericLength)

  // Since apply(Int) already exists, we are not allowed apply(T) since
  // they erase to the same thing.
  def apply(idx: Int): T = applyAt(fromInt(idx))
  def applyAt(idx: T): T = {
    if (idx < zero || idx >= genericLength) throw new IndexOutOfBoundsException(idx.toString)
    start + (idx * step)
  }

  // The contains situation makes for some interesting code.
  // This attempts to check containerhood in a range-sensible way, but
  // falls back on super.contains if the cast ends up failing.
  // !!! [Martin] contains should only return `true' for numbers of the form start + n * step.
  override def contains(_x: Any): Boolean = {
    def doContains = {
      // checking for Int is important so for instance BigIntRange from
      // 1 to Googlefinity can see if 5 is in there without calling super.
      val x = _x match {
        case i: Int => fromInt(i)
        case _      => _x.asInstanceOf[T]
      }
      def matchesStep = (x - start) % step == zero
      def withinRange =
        if (step > zero) start <= x && x < trueEnd
        else start >= x && x > trueEnd

      withinRange && matchesStep
    }

    // !!! [Martin] That's too inefficient foir a core library class in my opinion:
    catching(classOf[ClassCastException]) opt doContains getOrElse super.contains(_x)

  }

  // Using trueEnd gives us Range(1, 10, 1).inclusive == Range(1, 11, 1)
  val hashValues = List(start, trueEnd, step)

  // [Martin] !!! this means that GenericRange(0, 0, 1) and GenericRange(0, -1, 1) are not equal,
  // which violates the sequence equality conventions. See Range.equals for how it needs to be done.
  override def equals(other: Any) = other match {
    case x: GenericRange[_] => this equalHashValues x
    case _                  => false
  }
}

private[scala] trait RangeToString[T] extends VectorView[T, collection.immutable.Vector[T]] {
  // The default toString() tries to print every element and will exhaust memory
  // if the Range is unduly large.  This interacts poorly with the REPL.
  override def toString() = {
    val MAX_PRINT = 512  // some arbitrary value
    val str = (this take MAX_PRINT).mkString(", ")

    if (length > MAX_PRINT) str.replaceAll("""\)$""", ", ...)")
    else str
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

