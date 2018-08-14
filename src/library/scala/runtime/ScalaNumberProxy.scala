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
import RuntimeProxy.Typed

private[runtime] trait RuntimeProxy extends Any {
  protected def self: Any

  override def hashCode: Int = self.hashCode
  override def equals(that: Any): Boolean = that match {
    case null  => false
    case _     =>
      val x = that.asInstanceOf[AnyRef]
      (x eq this.asInstanceOf[AnyRef]) || (x eq self.asInstanceOf[AnyRef]) || (x equals self)
  }
  override def toString = "" + self
}

object RuntimeProxy {
  private[runtime] trait Typed[T] extends Any with RuntimeProxy {
    protected def self: T
  }
}

/** Base classes for the Rich* wrappers of the primitive types.
 *  As with all classes in scala.runtime.*, this is not a supported API.
 *
 *  @author Paul Phillips
 *  @since   2.9
 */
trait ScalaNumberProxy[T] extends Any with ScalaNumericAnyConversions with Typed[T] with OrderedProxy[T] {
  protected implicit def num: Numeric[T]

  def underlying  = self.asInstanceOf[AnyRef]
  def doubleValue = num.toDouble(self)
  def floatValue  = num.toFloat(self)
  def longValue   = num.toLong(self)
  def intValue    = num.toInt(self)
  def byteValue   = intValue.toByte
  def shortValue  = intValue.toShort

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
  def isWhole = true
}
trait IntegralProxy[T] extends Any with ScalaWholeNumberProxy[T] with RangedProxy[T] {
  protected implicit def num: Integral[T]
  type ResultWithoutStep = NumericRange[T]

  def until(end: T): NumericRange.Exclusive[T]          = NumericRange(self, end, num.one)
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  def to(end: T): NumericRange.Inclusive[T]             = NumericRange.inclusive(self, end, num.one)
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}
trait FractionalProxy[T] extends Any with ScalaNumberProxy[T] {
  protected implicit def num: Fractional[T]

  def isWhole = false
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

