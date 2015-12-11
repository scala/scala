/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** The package object `scala.math` contains methods for performing basic
  * numeric operations such as elementary exponential, logarithmic, root and
  * trigonometric functions.
  */
package object math {
  /** The `double` value that is closer than any other to `e`, the base of
   *  the natural logarithms.
   */
  @inline final val E = java.lang.Math.E

  /** The `double` value that is closer than any other to `pi`, the ratio of
   *  the circumference of a circle to its diameter.
   */
  @inline final val Pi = java.lang.Math.PI

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

  /** Returns the natural logarithm of a `double` value.
   *
   *  @param  x the number to take the natural logarithm of
   *  @return the value `logₑ(x)` where `e` is Eulers number
   */
  def log(x: Double): Double = java.lang.Math.log(x)
  
  /** Returns the square root of a `double` value.
   * 
   * @param  x the number to take the square root of
   * @return the value √x
   */
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)

  def ceil(x: Double): Double  = java.lang.Math.ceil(x)
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

  /** There is no reason to round a `Long`, but this method prevents unintended conversion to `Float` followed by rounding to `Int`. */
  @deprecated("This is an integer type; there is no reason to round it.  Perhaps you meant to call this with a floating-point value?", "2.11.0")
  def round(x: Long): Long = x

  /** Returns the closest `Int` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Int`.
   *  @return the value of the argument rounded to the nearest `Int` value.
   */
  def round(x: Float): Int = java.lang.Math.round(x)
  
  /** Returns the closest `Long` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Long`.
   *  @return the value of the argument rounded to the nearest`long` value.
   */
  def round(x: Double): Long = java.lang.Math.round(x)

  def abs(x: Int): Int       = java.lang.Math.abs(x)
  def abs(x: Long): Long     = java.lang.Math.abs(x)
  def abs(x: Float): Float   = java.lang.Math.abs(x)
  def abs(x: Double): Double = java.lang.Math.abs(x)

  def max(x: Int, y: Int): Int          = java.lang.Math.max(x, y)
  def max(x: Long, y: Long): Long       = java.lang.Math.max(x, y)
  def max(x: Float, y: Float): Float    = java.lang.Math.max(x, y)
  def max(x: Double, y: Double): Double = java.lang.Math.max(x, y)

  def min(x: Int, y: Int): Int          = java.lang.Math.min(x, y)
  def min(x: Long, y: Long): Long       = java.lang.Math.min(x, y)
  def min(x: Float, y: Float): Float    = java.lang.Math.min(x, y)
  def min(x: Double, y: Double): Double = java.lang.Math.min(x, y)

  /** Note that these are not pure forwarders to the java versions.
   *  In particular, the return type of java.lang.Long.signum is Int,
   *  but here it is widened to Long so that each overloaded variant
   *  will return the same numeric type it is passed.
   */
  def signum(x: Int): Int       = java.lang.Integer.signum(x)
  def signum(x: Long): Long     = java.lang.Long.signum(x)
  def signum(x: Float): Float   = java.lang.Math.signum(x)
  def signum(x: Double): Double = java.lang.Math.signum(x)

  // -----------------------------------------------------------------------
  // root functions
  // -----------------------------------------------------------------------

  /** Returns the cube root of the given `Double` value. */
  def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

  // -----------------------------------------------------------------------
  // exponential functions
  // -----------------------------------------------------------------------

  /** Returns `exp(x) - 1`. */
  def expm1(x: Double): Double = java.lang.Math.expm1(x)

  // -----------------------------------------------------------------------
  // logarithmic functions
  // -----------------------------------------------------------------------

  /** Returns the natural logarithm of the sum of the given `Double` value and 1. */
  def log1p(x: Double): Double = java.lang.Math.log1p(x)

  /** Returns the base 10 logarithm of the given `Double` value. */
  def log10(x: Double): Double = java.lang.Math.log10(x)

  // -----------------------------------------------------------------------
  // trigonometric functions
  // -----------------------------------------------------------------------

  /** Returns the hyperbolic sine of the given `Double` value. */
  def sinh(x: Double): Double = java.lang.Math.sinh(x)

  /** Returns the hyperbolic cosine of the given `Double` value. */
  def cosh(x: Double): Double = java.lang.Math.cosh(x)

  /** Returns the hyperbolic tangent of the given `Double` value. */
  def tanh(x: Double):Double = java.lang.Math.tanh(x)

  // -----------------------------------------------------------------------
  // miscellaneous functions
  // -----------------------------------------------------------------------

  /** Returns the square root of the sum of the squares of both given `Double`
    * values without intermediate underflow or overflow.
    */
  def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)

  /** Returns the size of an ulp of the given `Double` value. */
  def ulp(x: Double): Double = java.lang.Math.ulp(x)

  /** Returns the size of an ulp of the given `Float` value. */
  def ulp(x: Float): Float = java.lang.Math.ulp(x)
}
