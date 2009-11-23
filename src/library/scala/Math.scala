/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/** The object <code>Math</code> contains methods for performing basic numeric
 *  operations such as the elementary exponential, logarithm, square root, and
 *  trigonometric functions.
 */
@deprecated("use scala.math package instead")
object Math {
  @deprecated("Use scala.Byte.MinValue instead")
  val MIN_BYTE = java.lang.Byte.MIN_VALUE

  @deprecated("Use scala.Byte.MaxValue instead")
  val MAX_BYTE = java.lang.Byte.MAX_VALUE

  @deprecated("Use scala.Short.MinValue instead")
  val MIN_SHORT = java.lang.Short.MIN_VALUE

  @deprecated("Use scala.Short.MaxValue instead")
  val MAX_SHORT = java.lang.Short.MAX_VALUE

  @deprecated("Use scala.Char.MinValue instead")
  val MIN_CHAR = java.lang.Character.MIN_VALUE

  @deprecated("Use scala.Char.MaxValue instead")
  val MAX_CHAR = java.lang.Character.MAX_VALUE

  @deprecated("Use scala.Int.MinValue instead")
  val MIN_INT = java.lang.Integer.MIN_VALUE

  @deprecated("Use scala.Int.MaxValue instead")
  val MAX_INT = java.lang.Integer.MAX_VALUE

  @deprecated("Use scala.Long.MinValue instead")
  val MIN_LONG = java.lang.Long.MIN_VALUE

  @deprecated("Use scala.Long.MaxValue instead")
  val MAX_LONG = java.lang.Long.MAX_VALUE

  /** The smallest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  @deprecated("Use scala.Float.MinValue instead")
  val MIN_FLOAT  = -java.lang.Float.MAX_VALUE

  /** The smallest difference between two values of <a href="Float.html" target="_self">scala.Float</a>. */
  @deprecated("Use scala.Float.Epsilon instead")
  val EPS_FLOAT  = java.lang.Float.MIN_VALUE

  /** The greatest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  @deprecated("Use scala.Float.MaxValue instead")
  val MAX_FLOAT  = java.lang.Float.MAX_VALUE

  /** A value of type <a href="Float.html" target="_self">scala.Float</a> that represents no number. */
  @deprecated("Use scala.Float.NaN instead")
  val NaN_FLOAT  = java.lang.Float.NaN

  /** Negative infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  @deprecated("Use scala.Float.NegativeInfinity instead")
  val NEG_INF_FLOAT = java.lang.Float.NEGATIVE_INFINITY

  /** Positive infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  @deprecated("Use scala.Float.PositiveInfinity instead")
  val POS_INF_FLOAT = java.lang.Float.POSITIVE_INFINITY

  /** The smallest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  @deprecated("Use scala.Double.MinValue instead")
  val MIN_DOUBLE = -java.lang.Double.MAX_VALUE

  /** The smallest difference between two values of <a href="Double.html" target="_self">scala.Double</a>. */
  @deprecated("Use scala.Double.Epsilon instead")
  val EPS_DOUBLE = java.lang.Double.MIN_VALUE

  /** The greatest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  @deprecated("Use scala.Double.MaxValue instead")
  val MAX_DOUBLE = java.lang.Double.MAX_VALUE

  /** A value of type <a href="Double.html" target="_self">scala.Double</a> that represents no number. */
  @deprecated("Use scala.Double.NaN instead")
  val NaN_DOUBLE = java.lang.Double.NaN

  /** Negative infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  @deprecated("Use scala.Double.NegativeInfinity instead")
  val NEG_INF_DOUBLE = java.lang.Double.NEGATIVE_INFINITY

  /** Positive infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  @deprecated("Use scala.Double.PositiveInfinity instead")
  val POS_INF_DOUBLE = java.lang.Double.POSITIVE_INFINITY

  /** The code from here down is cut/pasted from the math package object.
   *  It should properly be in a shared trait but as of this writing
   *  inherited members in package objects are not visible.
   */

  /*******************************************************************/

  /** The <code>double</code> value that is closer than any other to
   *  <code>e</code>, the base of the natural logarithms.
   */
  val E = java.lang.Math.E

  /** The <code>double</code> value that is closer than any other to
   *  <code>pi</code>, the ratio of the circumference of a circle to its
   *  diameter.
   */
  val Pi = java.lang.Math.PI

  /** Returns a <code>double</code> value with a positive sign, greater than
   *  or equal to <code>0.0</code> and less than <code>1.0</code>.
   */
  def random: Double = java.lang.Math.random()

  def sin(x: Double): Double = java.lang.Math.sin(x)
  def cos(x: Double): Double = java.lang.Math.cos(x)
  def tan(x: Double): Double = java.lang.Math.tan(x)
  def asin(x: Double): Double = java.lang.Math.asin(x)
  def acos(x: Double): Double = java.lang.Math.acos(x)
  def atan(x: Double): Double = java.lang.Math.atan(x)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle <code>x</code> in radians.
   */
  def toRadians(x: Double): Double = java.lang.Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle <code>x</code> in degrees.
   */
  def toDegrees(x: Double): Double = java.lang.Math.toDegrees(x)

  /** Returns Euler's number <code>e</code> raised to the power of a
   *  <code>double</code> value.
   *
   *  @param  x the exponent to raise <code>e</code> to.
   *  @return the value <code>e<sup>a</sup></code>, where <code>e</code>
   *          is the base of the natural logarithms.
   */
  def exp(x: Double): Double = java.lang.Math.exp(x)
  def log(x: Double): Double = java.lang.Math.log(x)
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)

  def ceil(x: Double): Double = java.lang.Math.ceil(x)
  def floor(x: Double): Double = java.lang.Math.floor(x)

  /** Returns the <code>double</code> value that is closest in value to the
   *  argument and is equal to a mathematical integer.
   *
   *  @param  x a <code>double</code> value
   *  @return the closest floating-point value to a that is equal to a
   *          mathematical integer.
   */
  def rint(x: Double): Double = java.lang.Math.rint(x)

  /** Converts rectangular coordinates <code>(x, y)</code> to polar
   *  <code>(r, theta)</code>.
   *
   *  @param  x the ordinate coordinate
   *  @param  y the abscissa coordinate
   *  @return the <em>theta</em> component of the point <code>(r, theta)</code>
   *          in polar coordinates that corresponds to the point
   *          <code>(x, y)</code> in Cartesian coordinates.
   */
  def atan2(y: Double, x: Double): Double = java.lang.Math.atan2(y, x)

  /** Returns the value of the first argument raised to the power of the
   *  second argument.
   *
   *  @param x the base.
   *  @param y the exponent.
   *  @return the value <code>x<sup>y</sup></code>.
   */
  def pow(x: Double, y: Double): Double = java.lang.Math.pow(x, y)

  /** Returns the closest <code>long</code> to the argument.
   *
   *  @param  x a floating-point value to be rounded to a <code>long</code>.
   *  @return the value of the argument rounded to the nearest
   *          <code>long</code> value.
   */
  def round(x: Float): Int = java.lang.Math.round(x)
  def round(x: Double): Long = java.lang.Math.round(x)
  def abs(x: Int): Int = java.lang.Math.abs(x)
  def abs(x: Long): Long = java.lang.Math.abs(x)
  def abs(x: Float): Float = java.lang.Math.abs(x)
  def abs(x: Double): Double = java.lang.Math.abs(x)

  def max(x: Int, y: Int): Int = java.lang.Math.max(x, y)
  def max(x: Long, y: Long): Long = java.lang.Math.max(x, y)
  def max(x: Float, y: Float): Float = java.lang.Math.max(x, y)
  def max(x: Double, y: Double): Double = java.lang.Math.max(x, y)

  def min(x: Int, y: Int): Int = java.lang.Math.min(x, y)
  def min(x: Long, y: Long): Long  = java.lang.Math.min(x, y)
  def min(x: Float, y: Float): Float  = java.lang.Math.min(x, y)
  def min(x: Double, y: Double): Double = java.lang.Math.min(x, y)

  def signum(x: Double): Double =
    if (x == 0d) 0d
    else if (x < 0) -1.0
    else if (x > 0) 1.0
    else x    // NaN

  def signum(x: Float): Float =
    if (x == 0f) 0f
    else if (x < 0) -1.0f
    else if (x > 0) 1.0f
    else x    // NaN

  def signum(x: Long): Long =
    if (x == 0l) 0l
    else if (x < 0) -1l
    else 1l

  def signum(x: Int): Int =
    if (x == 0) 0
    else if (x < 0) -1
    else 1
}
