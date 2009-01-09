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
  val MIN_BYTE   = System.Byte.MinValue
  /** The greatest possible value for <a href="Byte.html" target="_self">scala.Byte</a>. */
  val MAX_BYTE   = System.Byte.MaxValue

  /** The smallest possible value for <a href="Short.html" target="_self">scala.Short</a>. */
  val MIN_SHORT  = System.Int16.MinValue
  /** The greatest possible value for <a href="Short.html" target="_self">scala.Short</a>. */
  val MAX_SHORT  = System.Int16.MaxValue

  /** The smallest possible value for <a href="Char.html" target="_self">scala.Char</a>. */
  val MIN_CHAR   = System.Char.MinValue
  /** The greatest possible value for <a href="Char.html" target="_self">scala.Char</a>. */
  val MAX_CHAR   = System.Char.MaxValue

  /** The smallest possible value for <a href="Int.html" target="_self">scala.Int</a>. */
  val MIN_INT    = System.Int32.MinValue
  /** The greatest possible value for <a href="Int.html" target="_self">scala.Int</a>. */
  val MAX_INT    = System.Int32.MaxValue

  /** The smallest possible value for <a href="Long.html" target="_self">scala.Long</a>. */
  val MIN_LONG   = System.Int64.MinValue
  /** The greatest possible value for <a href="Long.html" target="_self">scala.Long</a>. */
  val MAX_LONG   = System.Int64.MaxValue

  /** The smallest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  val MIN_FLOAT  = System.Single.MinValue
  /** The smallest difference between two values of <a href="Float.html" target="_self">scala.Float</a>. */
  val EPS_FLOAT  = System.Single.Epsilon
  /** The greatest possible value for <a href="Float.html" target="_self">scala.Float</a>. */
  val MAX_FLOAT  = System.Single.MinValue
  /** A value of type <a href="Float.html" target="_self">scala.Float</a> that represents no number. */
  //val NaN_FLOAT  = System.Single.NaN
  /** Negative infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  //val NEG_INF_FLOAT = System.Double.NegativeInfinity
  /** Positive infinity of type <a href="Float.html" target="_self">scala.Float</a>. */
  //val POS_INF_FLOAT = System.Double.PositiveInfinity

  /** The smallest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  val MIN_DOUBLE = System.Double.MinValue
  /** The smallest difference between two values of <a href="Double.html" target="_self">scala.Double</a>. */
  val EPS_DOUBLE = System.Double.Epsilon
  /** The greatest possible value for <a href="Double.html" target="_self">scala.Double</a>. */
  val MAX_DOUBLE = System.Double.MaxValue
  /** A value of type <a href="Double.html" target="_self">scala.Double</a> that represents no number. */
  //val NaN_DOUBLE = System.Double.NaN
  /** Negative infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  //val NEG_INF_DOUBLE = System.Double.NegativeInfinity
  /** Positive infinity of type <a href="Double.html" target="_self">scala.Double</a>. */
  //val POS_INF_DOUBLE = System.Double.PositiveInfinity

  /** The <code>double</code> value that is closer than any other to
   *  <code>e</code>, the base of the natural logarithms.
   */

  val E = System.Math.E
  val Pi = System.Math.PI

  //def random: Double = System.Math.random()

  def sin(x: Double): Double = System.Math.Sin(x)
  def cos(x: Double): Double = System.Math.Cos(x)
  def tan(x: Double): Double = System.Math.Tan(x)
  def asin(x: Double): Double = System.Math.Asin(x)
  def acos(x: Double): Double = System.Math.Acos(x)
  def atan(x: Double): Double = System.Math.Atan(x)

  def toRadians(x: Double): Double = x * Pi / 180.0

  def toDegrees(x: Double): Double = x * 180.0 / Pi

  def exp(x: Double): Double = System.Math.Exp(x)
  def log(x: Double): Double = System.Math.Log(x)
  def sqrt(x: Double): Double = System.Math.Sqrt(x)
  def sqrt(x: Int): Int = System.Math.Sqrt(x.toDouble).toInt
  def IEEEremainder(x: Double, y: Double): Double = System.Math.IEEERemainder(x, y)

  def ceil(x: Double): Double = System.Math.Ceiling(x)
  def floor(x: Double): Double = System.Math.Floor(x)

  //def rint(x: Double): Double = System.Math.rint(x)
  def atan2(y: Double, x: Double): Double = System.Math.Atan2(y, x)
  def pow(x: Double, y: Double): Double = System.Math.Pow(x, y)
  def round(x: Float): Int = System.Math.Round(x).toInt
  def round(x: Double): Long = System.Math.Round(x).toLong

  def abs(x: Int): Int = System.Math.Abs(x)
  def abs(x: Long): Long = System.Math.Abs(x)
  def abs(x: Float): Float = System.Math.Abs(x)
  def abs(x: Double): Double = System.Math.Abs(x)

  def max(x: Int, y: Int): Int = System.Math.Max(x, y)
  def max(x: Long, y: Long): Long = System.Math.Max(x, y)
  def max(x: Float, y: Float): Float = System.Math.Max(x, y)
  def max(x: Double, y: Double): Double = System.Math.Max(x, y)

  def min(x: Int, y: Int): Int = System.Math.Min(x, y)
  def min(x: Long, y: Long): Long  = System.Math.Min(x, y)
  def min(x: Float, y: Float): Float  = System.Math.Min(x, y)
  def min(x: Double, y: Double): Double = System.Math.Min(x, y)

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
