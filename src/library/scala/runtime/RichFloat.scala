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
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians: Float = math.toRadians(self).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees: Float = math.toDegrees(self).toFloat

  // isNaN is provided by the implicit conversion to java.lang.Float
  // def isNaN: Boolean = java.lang.Float.isNaN(self)
  def isInfinity: Boolean = java.lang.Float.isInfinite(self)
  def isPosInfinity: Boolean = isInfinity && self > 0.0f
  def isNegInfinity: Boolean = isInfinity && self < 0.0f
}
