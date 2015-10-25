/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import scala.collection.immutable
import scala.math.ScalaNumericAnyConversions
import immutable.NumericRange
import Proxy.Typed

/** Base classes for the Rich* wrappers of the primitive types.
 *  As with all classes in scala.runtime.*, this is not a supported API.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
trait ScalaNumberProxy[T] extends Any with ScalaNumericAnyConversions with Typed[T] with OrderedProxy[T] {
  protected implicit def num: Numeric[T]

  def underlying()  = self.asInstanceOf[AnyRef]
  def doubleValue() = num.toDouble(self)
  def floatValue()  = num.toFloat(self)
  def longValue()   = num.toLong(self)
  def intValue()    = num.toInt(self)
  def byteValue()   = intValue().toByte
  def shortValue()  = intValue().toShort

  /** Returns `'''this'''` if `'''this''' < that` or `that` otherwise. */
  def min(that: T): T = num.min(self, that)
  /** Returns `'''this'''` if `'''this''' > that` or `that` otherwise. */
  def max(that: T): T = num.max(self, that)
  /** Returns the absolute value of `'''this'''`. */
  def abs             = num.abs(self)
  /** Returns the signum of `'''this'''`. */
  def signum          = num.signum(self)
}
trait ScalaWholeNumberProxy[T] extends Any with ScalaNumberProxy[T] {
  def isWhole() = true
}
trait IntegralProxy[T] extends Any with ScalaWholeNumberProxy[T] with RangedProxy[T] {
  protected implicit def num: Integral[T]
  type ResultWithoutStep = NumericRange[T]

  def until(end: T): NumericRange.Exclusive[T]          = NumericRange(self, end, num.one)
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  def to(end: T): NumericRange.Inclusive[T]             = NumericRange.inclusive(self, end, num.one)
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}
trait FractionalProxy[T] extends Any with ScalaNumberProxy[T] with RangedProxy[T] {
  protected implicit def num: Fractional[T]
  protected implicit def integralNum: Integral[T]

  /** In order to supply predictable ranges, we require an Integral[T] which provides
   *  us with discrete operations on the (otherwise fractional) T.  See Numeric.DoubleAsIfIntegral
   *  for an example.
   */
  type ResultWithoutStep = Range.Partial[T, NumericRange[T]]

  def isWhole() = false
  def until(end: T): ResultWithoutStep                  = new Range.Partial(NumericRange(self, end, _))
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  def to(end: T): ResultWithoutStep                     = new Range.Partial(NumericRange.inclusive(self, end, _))
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}

trait OrderedProxy[T] extends Any with Ordered[T] with Typed[T] {
  protected def ord: Ordering[T]

  def compare(y: T) = ord.compare(self, y)
}
trait RangedProxy[T] extends Any with Typed[T] {
  type ResultWithoutStep

  def until(end: T): ResultWithoutStep
  def until(end: T, step: T): immutable.IndexedSeq[T]
  def to(end: T): ResultWithoutStep
  def to(end: T, step: T): immutable.IndexedSeq[T]
}

