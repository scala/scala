/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichFloat(val self: Float) extends FractionalProxy[Float] {
  protected val integralNum = Numeric.FloatAsIfIntegral

  def round: Int   = math.round(self)
  def ceil: Float  = math.ceil(self).toFloat
  def floor: Float = math.floor(self).toFloat

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians: Float = math.toRadians(self).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees: Float = math.toDegrees(self).toFloat

  // isNaN is provided by the implicit conversion to java.lang.Float
  // def isNaN: Boolean = java.lang.Float.isNaN(self)
  def isInfinity: Boolean = java.lang.Float.isInfinite(self)
  def isPosInfinity: Boolean = isInfinity && self > 0.0f
  def isNegInfinity: Boolean = isInfinity && self < 0.0f

  override def isValidByte = self.toByte.toFloat == self
  override def isValidShort = self.toShort.toFloat == self
  override def isValidChar = self.toChar.toFloat == self
  override def isValidInt = { val i = self.toInt; i.toFloat == self && i != Int.MaxValue }
  // override def isValidLong = { val l = self.toLong; l.toFloat == self && l != Long.MaxValue }
  // override def isValidFloat = !java.lang.Float.isNaN(self)
  // override def isValidDouble = !java.lang.Float.isNaN(self)
  override def isWhole = {
    val l = self.toLong
    l.toFloat == self || l == Long.MaxValue && self < Float.PositiveInfinity || l == Long.MinValue && self > Float.NegativeInfinity
  }
}
