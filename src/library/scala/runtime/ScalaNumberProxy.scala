/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.collection.{ mutable, immutable }
import math.ScalaNumericConversions
import immutable.NumericRange
import Proxy.Typed

/** Base classes for the Rich* wrappers of the primitive types.
 *  As with all classes in scala.runtime.*, this is not a supported API.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
abstract class ScalaNumberProxy[T: Numeric] extends ScalaNumericConversions with Typed[T] with OrderedProxy[T] {
  private val num = implicitly[Numeric[T]]
  protected val ord: Ordering[T] = num

  def underlying()  = self.asInstanceOf[AnyRef]
  def doubleValue() = num.toDouble(self)
  def floatValue()  = num.toFloat(self)
  def longValue()   = num.toLong(self)
  def intValue()    = num.toInt(self)

  def min(that: T): T = num.min(self, that)
  def max(that: T): T = num.max(self, that)
  def abs             = num.abs(self)
  def signum          = num.signum(self)
}
abstract class ScalaWholeNumberProxy[T: Numeric] extends ScalaNumberProxy[T] {
  def isWhole() = true
}
abstract class IntegralProxy[T : Integral] extends ScalaWholeNumberProxy[T] with RangedProxy[T] {
  private lazy val num = implicitly[Integral[T]]
  type ResultWithoutStep = NumericRange[T]

  def until(end: T): NumericRange.Exclusive[T]          = NumericRange(self, end, num.one)
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  def to(end: T): NumericRange.Inclusive[T]             = NumericRange.inclusive(self, end, num.one)
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}
abstract class FractionalProxy[T : Fractional] extends ScalaNumberProxy[T] with RangedProxy[T] {
  def isWhole() = false

  /** In order to supply predictable ranges, we require an Integral[T] which provides
   *  us with discrete operations on the (otherwise fractional) T.  See Numeric.DoubleAsIfIntegral
   *  for an example.
   */
  protected implicit def integralNum: Integral[T]
  private lazy val num = implicitly[Fractional[T]]
  type ResultWithoutStep = Range.Partial[T, NumericRange[T]]

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

