/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala

/** Common code between the deprecated `scala.Math` object and
 *  the `scala.math` package object.
 */
private[scala] class MathCommon {
  /** The `double` value that is closer than any other to `e`, the base of
   *  the natural logarithms.
   */
  val E = java.lang.Math.E

  /** The `double` value that is closer than any other to `pi`, the ratio of
   *  the circumference of a circle to its diameter.
   */
  val Pi = java.lang.Math.PI

  /** Returns a `double` value with a positive sign, greater than or equal
   *  to `0.0` and less than `1.0`.
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
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians(x: Double): Double = java.lang.Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees(x: Double): Double = java.lang.Math.toDegrees(x)

  /** Returns Euler's number `e` raised to the power of a `double` value.
   *
   *  @param  x the exponent to raise `e` to.
   *  @return the value `e^a^`, where `e` is the base of the natural
   *          logarithms.
   */
  def exp(x: Double): Double = java.lang.Math.exp(x)
  def log(x: Double): Double = java.lang.Math.log(x)
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)

  def ceil(x: Double): Double = java.lang.Math.ceil(x)
  def floor(x: Double): Double = java.lang.Math.floor(x)

  /** Returns the `double` value that is closest in value to the
   *  argument and is equal to a mathematical integer.
   *
   *  @param  x a `double` value
   *  @return the closest floating-point value to a that is equal to a
   *          mathematical integer.
   */
  def rint(x: Double): Double = java.lang.Math.rint(x)

  /** Converts rectangular coordinates `(x, y)` to polar `(r, theta)`.
   *
   *  @param  x the ordinate coordinate
   *  @param  y the abscissa coordinate
   *  @return the ''theta'' component of the point `(r, theta)` in polar
   *          coordinates that corresponds to the point `(x, y)` in
   *          Cartesian coordinates.
   */
  def atan2(y: Double, x: Double): Double = java.lang.Math.atan2(y, x)

  /** Returns the value of the first argument raised to the power of the
   *  second argument.
   *
   *  @param x the base.
   *  @param y the exponent.
   *  @return the value `x^y^`.
   */
  def pow(x: Double, y: Double): Double = java.lang.Math.pow(x, y)

  /** Returns the closest `long` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `long`.
   *  @return the value of the argument rounded to the nearest`long` value.
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
