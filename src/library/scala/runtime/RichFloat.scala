/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

final class RichFloat(val self: Float) extends AnyVal with FractionalProxy[Float] {
  protected def num         = scala.math.Numeric.FloatIsFractional
  protected def ord         = scala.math.Ordering.Float
  protected def integralNum = scala.math.Numeric.FloatAsIfIntegral

  override def doubleValue() = self.toDouble
  override def floatValue()  = self
  override def longValue()   = self.toLong
  override def intValue()    = self.toInt
  override def byteValue()   = self.toByte
  override def shortValue()  = self.toShort

  override def isWhole = {
    val l = self.toLong
    l.toFloat == self || l == Long.MaxValue && self < Float.PositiveInfinity || l == Long.MinValue && self > Float.NegativeInfinity
  }
  override def isValidByte  = self.toByte.toFloat == self
  override def isValidShort = self.toShort.toFloat == self
  override def isValidChar  = self.toChar.toFloat == self
  override def isValidInt   = { val i = self.toInt; i.toFloat == self && i != Int.MaxValue }
  // override def isValidLong = { val l = self.toLong; l.toFloat == self && l != Long.MaxValue }
  // override def isValidFloat = !java.lang.Float.isNaN(self)
  // override def isValidDouble = !java.lang.Float.isNaN(self)

  def isNaN: Boolean         = java.lang.Float.isNaN(self)
  def isInfinity: Boolean    = java.lang.Float.isInfinite(self)
  def isPosInfinity: Boolean = Float.PositiveInfinity == self
  def isNegInfinity: Boolean = Float.NegativeInfinity == self

  override def abs: Float              = math.abs(self)
  override def max(that: Float): Float = math.max(self, that)
  override def min(that: Float): Float = math.min(self, that)
  override def signum: Int             = math.signum(self).toInt  // !!! NaN

  def round: Int   = math.round(self)
  def ceil: Float  = math.ceil(self.toDouble).toFloat
  def floor: Float = math.floor(self.toDouble).toFloat

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians: Float = math.toRadians(self.toDouble).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees: Float = math.toDegrees(self.toDouble).toFloat
}
