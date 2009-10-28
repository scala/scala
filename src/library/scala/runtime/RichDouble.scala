/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import scala.collection.immutable.{Range, GenericRange}

final class RichDouble(x: Double) extends Proxy with Ordered[Double] {
  // Proxy.self
  def self: Any = x

  def compare(y: Double): Int = java.lang.Double.compare(x, y)

  def min(y: Double): Double = Math.min(x, y)
  def max(y: Double): Double = Math.max(x, y)
  def abs: Double = Math.abs(x)

  def round: Long = Math.round(x)
  def ceil: Double = Math.ceil(x)
  def floor: Double = Math.floor(x)

  /** See <code>BigDecimal.until</code>. */
  def until(end: Double): Range.Partial[Double, GenericRange[Double]] =
    new Range.Partial(until(end, _))

  /** See <code>BigDecimal.until</code>. */
  def until(end: Double, step: Double): GenericRange[Double] =
    Range.Double(x, end, step)

  /** See <code>BigDecimal.to</code>. */
  def to(end: Double): Range.Partial[Double, GenericRange[Double]] =
    new Range.Partial(to(end, _))

  /** See <code>BigDecimal.to</code>. */
  def to(end: Double, step: Double): GenericRange[Double] =
    Range.Double.inclusive(x, end, step)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle <code>x</code> in radians.
   */
  def toRadians: Double = Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle <code>x</code> in degrees.
   */
  def toDegrees: Double = Math.toDegrees(x)

  // isNaN is provided by the implicit conversion to java.lang.Double
  // def isNaN: Boolean = java.lang.Double.isNaN(x)
  def isInfinity: Boolean = java.lang.Double.isInfinite(x)
  def isPosInfinity: Boolean = isInfinity && x > 0.0
  def isNegInfinity: Boolean = isInfinity && x < 0.0

}
