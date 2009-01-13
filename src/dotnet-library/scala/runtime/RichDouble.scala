/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichDouble(x: Double) extends Proxy with Ordered[Double] {

  // Proxy.self
  def self: Any = x

  // Ordered[Double].compare
  def compare(y: Double): Int = if (x < y) -1 else if (x > y) 1 else 0

  def min(y: Double): Double = Math.min(x, y)
  def max(y: Double): Double = Math.max(x, y)
  def abs: Double = Math.abs(x)

  def round: Long = Math.round(x)
  def ceil: Double = Math.ceil(x)
  def floor: Double = Math.floor(x)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle <code>x</code> in radians.
   */
  def toRadians: Double = Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle <code>x</code> in degrees.
   */
  def toDegrees: Double = Math.toDegrees(x)

  def isNaN: Boolean = System.Double.IsNaN(x)
  def isInfinity: Boolean = System.Double.IsInfinity(x)
  def isPosInfinity: Boolean = System.Double.IsPositiveInfinity(x)
  def isNegInfinity: Boolean = System.Double.IsNegativeInfinity(x)

}
