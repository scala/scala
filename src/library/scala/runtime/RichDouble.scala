/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

final class RichDouble(val self: Double) extends AnyVal with FractionalProxy[Double] {
  protected def num = scala.math.Numeric.DoubleIsFractional
  protected def ord = scala.math.Ordering.Double
  protected def integralNum = scala.math.Numeric.DoubleAsIfIntegral

  override def doubleValue() = self
  override def floatValue()  = self.toFloat
  override def longValue()   = self.toLong
  override def intValue()    = self.toInt
  override def byteValue()   = self.toByte
  override def shortValue()  = self.toShort

  override def isWhole = {
    val l = self.toLong
    l.toDouble == self || l == Long.MaxValue && self < Double.PositiveInfinity || l == Long.MinValue && self > Double.NegativeInfinity
  }
  override def isValidByte  = self.toByte.toDouble == self
  override def isValidShort = self.toShort.toDouble == self
  override def isValidChar  = self.toChar.toDouble == self
  override def isValidInt   = self.toInt.toDouble == self
  // override def isValidLong = { val l = self.toLong; l.toDouble == self && l != Long.MaxValue }
  // override def isValidFloat = self.toFloat.toDouble == self
  // override def isValidDouble = !java.lang.Double.isNaN(self)

  def isNaN: Boolean         = java.lang.Double.isNaN(self)
  def isInfinity: Boolean    = java.lang.Double.isInfinite(self)
  def isPosInfinity: Boolean = Double.PositiveInfinity == self
  def isNegInfinity: Boolean = Double.NegativeInfinity == self

  override def abs: Double               = math.abs(self)
  override def max(that: Double): Double = math.max(self, that)
  override def min(that: Double): Double = math.min(self, that)
  override def signum: Int               = math.signum(self).toInt  // !!! NaN

  def round: Long   = math.round(self)
  def ceil: Double  = math.ceil(self)
  def floor: Double = math.floor(self)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle x in radians.
   */
  def toRadians: Double = math.toRadians(self)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *  @return the measurement of the angle x in degrees.
   */
  def toDegrees: Double = math.toDegrees(self)
}
