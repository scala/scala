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
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle x in radians.
   */
  def toRadians: Double = math.toRadians(self)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *  @param  x angle, in radians
   *  @return the measurement of the angle x in degrees.
   */
  def toDegrees: Double = math.toDegrees(self)

  // isNaN is provided by the implicit conversion to java.lang.Double
  // def isNaN: Boolean = java.lang.Double.isNaN(self)
  def isInfinity: Boolean = java.lang.Double.isInfinite(self)
  def isPosInfinity: Boolean = isInfinity && self > 0.0
  def isNegInfinity: Boolean = isInfinity && self < 0.0
}
