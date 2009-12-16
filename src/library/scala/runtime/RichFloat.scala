/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichFloat(x: Float) extends Proxy with Ordered[Float] {

  // Proxy.self
  def self: Any = x

  // Ordered[Float].compare
  //def compare(y: Float): Int = if (x < y) -1 else if (x > y) 1 else 0
  def compare(y: Float): Int = java.lang.Float.compare(x, y)

  def min(y: Float) = math.min(x, y)
  def max(y: Float) = math.max(x, y)
  def abs: Float = math.abs(x)

  def round: Int = math.round(x)
  def ceil: Float = math.ceil(x).toFloat
  def floor: Float = math.floor(x).toFloat

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle <code>x</code> in radians.
   */
  def toRadians: Float = math.toRadians(x).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle <code>x</code> in degrees.
   */
  def toDegrees: Float = math.toDegrees(x).toFloat

  // isNaN is provided by the implicit conversion to java.lang.Float
  // def isNaN: Boolean = java.lang.Float.isNaN(x)
  def isInfinity: Boolean = java.lang.Float.isInfinite(x)
  def isPosInfinity: Boolean = isInfinity && x > 0.0f
  def isNegInfinity: Boolean = isInfinity && x < 0.0f

}
