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
object Math {

  /** The smallest possible value for <a href="Byte.html" target="_self">scala.Byte</a>. */
  val MIN_BYTE   = java.lang.Byte.MIN_VALUE
  /** The greatest possible value for <a href="Byte.html" target="_self">scala.Byte</a>. */
  val MAX_BYTE   = java.lang.Byte.MAX_VALUE

  /** The smallest possible value for <a href="Short.html" target="_self">scala.Short</a>. */
  val MIN_SHORT  = java.lang.Short.MIN_VALUE
  /** The greatest possible value for <a href="Short.html" target="_self">scala.Short</a>. */
  val MAX_SHORT  = java.lang.Short.MAX_VALUE

  /** The smallest possible value for <a href="Char.html" target="_self">scala.Char</a>. */
  val MIN_CHAR   = java.lang.Character.MIN_VALUE
  /** The greatest possible value for <a href="Char.html" target="_self">scala.Char</a>. */
  val MAX_CHAR   = java.lang.Character.MAX_VALUE

  /** The smallest possible value for <a href="Int.html" target="_self">scala.Int</a>. */
  val MIN_INT    = java.lang.Integer.MIN_VALUE
  /** The greatest possible value for <a href="Int.html" target="_self">scala.Int</a>. */
  val MAX_INT    = java.lang.Integer.MAX_VALUE

  /** The smallest possible value for <a href="Long.html" target="_self">scala.Long</a>. */
  val MIN_LONG   = java.lang.Long.MIN_VALUE
  /** The greatest possible value for <a href="Long.html" target="_self">scala.Long</a>. */
  val MAX_LONG   = java.lang.Long.MAX_VALUE

  /** The smallest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  val MIN_FLOAT  = -java.lang.Float.MAX_VALUE
  /** The smallest difference between two values of <a href="Float.html" target="_self">scala.Float</a>. */
  val EPS_FLOAT  = java.lang.Float.MIN_VALUE
  /** The greatest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  val MAX_FLOAT  = java.lang.Float.MAX_VALUE
  /** A value of type <a href="Float.html" target="_self">scala.Float</a> that represents no number. */
  val NaN_FLOAT  = java.lang.Float.NaN
  /** Negative infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  val NEG_INF_FLOAT = java.lang.Float.NEGATIVE_INFINITY
  /** Positive infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  val POS_INF_FLOAT = java.lang.Float.POSITIVE_INFINITY

  /** The smallest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  val MIN_DOUBLE = -java.lang.Double.MAX_VALUE
  /** The smallest difference between two values of <a href="Double.html" target="_self">scala.Double</a>. */
  val EPS_DOUBLE = java.lang.Double.MIN_VALUE
  /** The greatest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  val MAX_DOUBLE = java.lang.Double.MAX_VALUE
  /** A value of type <a href="Double.html" target="_self">scala.Double</a> that represents no number. */
  val NaN_DOUBLE = java.lang.Double.NaN
  /** Negative infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  val NEG_INF_DOUBLE = java.lang.Double.NEGATIVE_INFINITY
  /** Positive infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  val POS_INF_DOUBLE = java.lang.Double.POSITIVE_INFINITY

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
  def sqrt(x: Int): Int = java.lang.Math.sqrt(x.toDouble).toInt
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

  def signum(x: Double): Double = x match { case 0 => 0
                                            case y if y < 0 => -1.0
                                            case y if y > 0 => 1.0 }
  def signum(x: Float): Float = x match { case 0f => 0f
                                          case y if y < 0f => -1.0f
                                          case y if y > 0f => 1.0f }
  def signum(x: Long): Long = x match { case 0l => 0l
                                        case y if y < 0l => -1l
                                        case y if y > 0l => 1l }
  def signum(x: Int): Int = x match { case 0 => 0
                                      case y if y < 0 => -1
                                      case y if y > 0 => 1}

  // from Java 1.5
//   def log10(x: Double): Double = java.lang.Math.log10(x)
//   def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

//   def ulp(x: Double): Double = java.lang.Math.ulp(x)
//   def ulp(x: Float): Float = java.lang.Math.ulp(x)
//   def sinh(x: Double): Double = java.lang.Math.sinh(x)
//   def cosh(x: Double): Double = java.lang.Math.cosh(x)
//   def tanh(x: Double):Double = java.lang.Math.tanh(x)
//   def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)
//   def expm1(x: Double): Double = java.lang.Math.expm1(x)
//   def log1p(x: Double): Double = java.lang.Math.log1p(x)

}
