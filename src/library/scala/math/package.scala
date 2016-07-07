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
  *
  * All methods forward to [[java.lang.Math]] unless otherwise noted.
  *
  * @see [[java.lang.Math]]
  *
  * @groupname math-const Mathematical Constants
  * @groupprio math-const 10
  *
  * @groupname minmax Minimum and Maximum
  * @groupdesc minmax Find the min or max of two numbers. Note: [[scala.collection.TraversableOnce]] has
  *           min and max methods which determine the min or max of a collection.
  * @groupprio minmax 20
  *
  * @groupname rounding Rounding
  * @groupprio rounding 30
  *
  * @groupname explog Exponential and Logarithmic
  * @groupprio explog 40
  *
  * @groupname trig Trigonometric
  * @groupdesc trig Arguments in radians
  * @groupprio trig 50
  *
  * @groupname angle-conversion Angular Measurement Conversion
  * @groupprio angle-conversion 60
  *
  * @groupname hyperbolic Hyperbolic
  * @groupprio hyperbolic 70
  *
  * @groupname abs Absolute Values
  * @groupdesc abs Determine the magnitude of a value by discarding the sign. Results are >= 0.
  * @groupprio abs 80
  *
  * @groupname signum Signs
  * @groupdesc signum Extract the sign of a value. Results are -1, 0 or 1.
  * Note that these are not pure forwarders to the java versions.
  * In particular, the return type of java.lang.Long.signum is Int,
  * but here it is widened to Long so that each overloaded variant
  * will return the same numeric type it is passed.
  * @groupprio signum 90
  *
  * @groupname root-extraction Root Extraction
  * @groupprio root-extraction 100
  *
  * @groupname polar-coords Polar Coordinates
  * @groupprio polar-coords 110
  *
  * @groupname ulp Unit of Least Precision
  * @groupprio ulp 120
  *
  * @groupname randomisation Pseudo Random Number Generation
  * @groupprio randomisation 130
  */
package object math {
  /** The `Double` value that is closer than any other to `e`, the base of
   *  the natural logarithms.
   *  @group math-const
   */
  @inline final val E = java.lang.Math.E

  /** The `Double` value that is closer than any other to `pi`, the ratio of
   *  the circumference of a circle to its diameter.
   *  @group math-const
   */
  @inline final val Pi = java.lang.Math.PI

  /** Returns a `Double` value with a positive sign, greater than or equal
   *  to `0.0` and less than `1.0`.
   *
   *  @group randomisation
   */
  def random(): Double = java.lang.Math.random()

  /**  @group trig */
  def sin(x: Double): Double = java.lang.Math.sin(x)
  /**  @group trig */
  def cos(x: Double): Double = java.lang.Math.cos(x)
  /**  @group trig */
  def tan(x: Double): Double = java.lang.Math.tan(x)
  /**  @group trig */
  def asin(x: Double): Double = java.lang.Math.asin(x)
  /**  @group trig */
  def acos(x: Double): Double = java.lang.Math.acos(x)
  /**  @group trig */
  def atan(x: Double): Double = java.lang.Math.atan(x)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle `x` in radians.
   *  @group angle-conversion
   */
  def toRadians(x: Double): Double = java.lang.Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle `x` in degrees.
   *  @group angle-conversion
   */
  def toDegrees(x: Double): Double = java.lang.Math.toDegrees(x)

  /** Converts rectangular coordinates `(x, y)` to polar `(r, theta)`.
   *
   *  @param  x the ordinate coordinate
   *  @param  y the abscissa coordinate
   *  @return the ''theta'' component of the point `(r, theta)` in polar
   *          coordinates that corresponds to the point `(x, y)` in
   *          Cartesian coordinates.
   *  @group polar-coords
   */
  def atan2(y: Double, x: Double): Double = java.lang.Math.atan2(y, x)

  /** Returns the square root of the sum of the squares of both given `Double`
    * values without intermediate underflow or overflow.
    *
    * The ''r'' component of the point `(r, theta)` in polar
    * coordinates that corresponds to the point `(x, y)` in
    * Cartesian coordinates.
    * @group polar-coords
    */
  def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)

  // -----------------------------------------------------------------------
  // rounding functions
  // -----------------------------------------------------------------------

  /** @group rounding */
  def ceil(x: Double): Double  = java.lang.Math.ceil(x)
  /** @group rounding */
  def floor(x: Double): Double = java.lang.Math.floor(x)

  /** Returns the `Double` value that is closest in value to the
    *  argument and is equal to a mathematical integer.
    *
    *  @param  x a `Double` value
    *  @return the closest floating-point value to a that is equal to a
    *          mathematical integer.
    *  @group rounding
    */
  def rint(x: Double): Double = java.lang.Math.rint(x)

  /** There is no reason to round a `Long`, but this method prevents unintended conversion to `Float` followed by rounding to `Int`.
   *
   *  @note Does not forward to [[java.lang.Math]]
   *  @group rounding
   */
  @deprecated("This is an integer type; there is no reason to round it. Perhaps you meant to call this with a floating-point value?", "2.11.0")
  def round(x: Long): Long = x

  /** Returns the closest `Int` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Int`.
   *  @return the value of the argument rounded to the nearest `Int` value.
   *  @group rounding
   */
  def round(x: Float): Int = java.lang.Math.round(x)

  /** Returns the closest `Long` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Long`.
   *  @return the value of the argument rounded to the nearest`long` value.
   *  @group rounding
   */
  def round(x: Double): Long = java.lang.Math.round(x)

  /** @group abs */
  def abs(x: Int): Int       = java.lang.Math.abs(x)
  /** @group abs */
  def abs(x: Long): Long     = java.lang.Math.abs(x)
  /** @group abs */
  def abs(x: Float): Float   = java.lang.Math.abs(x)
  /** @group abs */
  def abs(x: Double): Double = java.lang.Math.abs(x)

  /** @group minmax */
  def max(x: Int, y: Int): Int          = java.lang.Math.max(x, y)
  /** @group minmax */
  def max(x: Long, y: Long): Long       = java.lang.Math.max(x, y)
  /** @group minmax */
  def max(x: Float, y: Float): Float    = java.lang.Math.max(x, y)
  /** @group minmax */
  def max(x: Double, y: Double): Double = java.lang.Math.max(x, y)

  /** @group minmax */
  def min(x: Int, y: Int): Int          = java.lang.Math.min(x, y)
  /** @group minmax */
  def min(x: Long, y: Long): Long       = java.lang.Math.min(x, y)
  /** @group minmax */
  def min(x: Float, y: Float): Float    = java.lang.Math.min(x, y)
  /** @group minmax */
  def min(x: Double, y: Double): Double = java.lang.Math.min(x, y)

  /** @group signum
    * @note Forwards to [[java.lang.Integer]]
    */
  def signum(x: Int): Int       = java.lang.Integer.signum(x)
  /** @group signum
    * @note Forwards to [[java.lang.Long]]
    */
  def signum(x: Long): Long     = java.lang.Long.signum(x)
  /** @group signum */
  def signum(x: Float): Float   = java.lang.Math.signum(x)
  /** @group signum */
  def signum(x: Double): Double = java.lang.Math.signum(x)

  // -----------------------------------------------------------------------
  // root functions
  // -----------------------------------------------------------------------

  /** Returns the square root of a `Double` value.
    *
    * @param  x the number to take the square root of
    * @return the value √x
    * @group root-extraction
    */
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)

  /** Returns the cube root of the given `Double` value.
    *
    * @param  x the number to take the cube root of
    * @return the value ∛x
    * @group root-extraction
    */
  def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

  // -----------------------------------------------------------------------
  // exponential functions
  // -----------------------------------------------------------------------

  /** Returns the value of the first argument raised to the power of the
    *  second argument.
    *
    *  @param x the base.
    *  @param y the exponent.
    *  @return the value `x^y^`.
    *  @group explog
    */
  def pow(x: Double, y: Double): Double = java.lang.Math.pow(x, y)

  /** Returns Euler's number `e` raised to the power of a `Double` value.
    *
    *  @param  x the exponent to raise `e` to.
    *  @return the value `e^a^`, where `e` is the base of the natural
    *          logarithms.
    *  @group explog
    */
  def exp(x: Double): Double = java.lang.Math.exp(x)

  /** Returns `exp(x) - 1`.
    *  @group explog
    */
  def expm1(x: Double): Double = java.lang.Math.expm1(x)

  // -----------------------------------------------------------------------
  // logarithmic functions
  // -----------------------------------------------------------------------

  /** Returns the natural logarithm of a `Double` value.
    *
    *  @param  x the number to take the natural logarithm of
    *  @return the value `logₑ(x)` where `e` is Eulers number
    *  @group explog
    */
  def log(x: Double): Double = java.lang.Math.log(x)

  /** Returns the natural logarithm of the sum of the given `Double` value and 1.
    *  @group explog
    */
  def log1p(x: Double): Double = java.lang.Math.log1p(x)

  /** Returns the base 10 logarithm of the given `Double` value.
    *  @group explog
    */
  def log10(x: Double): Double = java.lang.Math.log10(x)

  // -----------------------------------------------------------------------
  // trigonometric functions
  // -----------------------------------------------------------------------

  /** Returns the hyperbolic sine of the given `Double` value.
    * @group hyperbolic
    */
  def sinh(x: Double): Double = java.lang.Math.sinh(x)

  /** Returns the hyperbolic cosine of the given `Double` value.
    * @group hyperbolic
    */
  def cosh(x: Double): Double = java.lang.Math.cosh(x)

  /** Returns the hyperbolic tangent of the given `Double` value.
    * @group hyperbolic
    */
  def tanh(x: Double):Double = java.lang.Math.tanh(x)

  // -----------------------------------------------------------------------
  // miscellaneous functions
  // -----------------------------------------------------------------------

  /** Returns the size of an ulp of the given `Double` value.
    * @group ulp
    */
  def ulp(x: Double): Double = java.lang.Math.ulp(x)

  /** Returns the size of an ulp of the given `Float` value.
    * @group ulp
    */
  def ulp(x: Float): Float = java.lang.Math.ulp(x)

  /** @group rounding */
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)
}
