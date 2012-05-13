/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichDouble(val self: Double) extends FractionalProxy[Double] {
  protected val integralNum = Numeric.DoubleAsIfIntegral

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

  // isNaN is provided by the implicit conversion to java.lang.Double
  // def isNaN: Boolean = java.lang.Double.isNaN(self)
  def isInfinity: Boolean = java.lang.Double.isInfinite(self)
  def isPosInfinity: Boolean = isInfinity && self > 0.0
  def isNegInfinity: Boolean = isInfinity && self < 0.0

  override def isValidByte = self.toByte.toDouble == self
  override def isValidShort = self.toShort.toDouble == self
  override def isValidChar = self.toChar.toDouble == self
  override def isValidInt = self.toInt.toDouble == self
  // override def isValidLong = { val l = self.toLong; l.toDouble == self && l != Long.MaxValue }
  // override def isValidFloat = self.toFloat.toDouble == self
  // override def isValidDouble = !java.lang.Double.isNaN(self)
  override def isWhole = {
    val l = self.toLong
    l.toDouble == self || l == Long.MaxValue && self < Double.PositiveInfinity || l == Long.MinValue && self > Double.NegativeInfinity
  }
}
