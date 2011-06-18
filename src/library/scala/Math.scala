/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala

/** The object <code>Math</code> contains methods for performing basic numeric
 *  operations such as the elementary exponential, logarithm, square root, and
 *  trigonometric functions.
 */
@deprecated("use the scala.math package object instead.\n(Example package object usage: scala.math.Pi )", "2.8.0")
object Math extends MathCommon {
  @deprecated("Use `scala.Byte.MinValue` instead", "2.8.0")
  val MIN_BYTE = java.lang.Byte.MIN_VALUE

  @deprecated("Use `scala.Byte.MaxValue` instead", "2.8.0")
  val MAX_BYTE = java.lang.Byte.MAX_VALUE

  @deprecated("Use `scala.Short.MinValue` instead", "2.8.0")
  val MIN_SHORT = java.lang.Short.MIN_VALUE

  @deprecated("Use `scala.Short.MaxValue` instead", "2.8.0")
  val MAX_SHORT = java.lang.Short.MAX_VALUE

  @deprecated("Use `scala.Char.MinValue` instead", "2.8.0")
  val MIN_CHAR = java.lang.Character.MIN_VALUE

  @deprecated("Use `scala.Char.MaxValue` instead", "2.8.0")
  val MAX_CHAR = java.lang.Character.MAX_VALUE

  @deprecated("Use `scala.Int.MinValue` instead", "2.8.0")
  val MIN_INT = java.lang.Integer.MIN_VALUE

  @deprecated("Use `scala.Int.MaxValue` instead", "2.8.0")
  val MAX_INT = java.lang.Integer.MAX_VALUE

  @deprecated("Use `scala.Long.MinValue` instead", "2.8.0")
  val MIN_LONG = java.lang.Long.MIN_VALUE

  @deprecated("Use `scala.Long.MaxValue` instead", "2.8.0")
  val MAX_LONG = java.lang.Long.MAX_VALUE

  /** The smallest possible value for [[scala.Float]]. */
  @deprecated("Use `scala.Float.MinValue` instead", "2.8.0")
  val MIN_FLOAT  = -java.lang.Float.MAX_VALUE

  /** The smallest difference between two values of [[scala.Float]]. */
  @deprecated("Use `scala.Float.MinPositiveValue` instead", "2.8.0")
  val EPS_FLOAT  = java.lang.Float.MIN_VALUE

  /** The greatest possible value for [[scala.Float]]. */
  @deprecated("Use `scala.Float.MaxValue` instead", "2.8.0")
  val MAX_FLOAT  = java.lang.Float.MAX_VALUE

  /** A value of type [[scala.Float]] that represents no number. */
  @deprecated("Use `scala.Float.NaN` instead", "2.8.0")
  val NaN_FLOAT  = java.lang.Float.NaN

  /** Negative infinity of type [[scala.Float]]. */
  @deprecated("Use `scala.Float.NegativeInfinity` instead", "2.8.0")
  val NEG_INF_FLOAT = java.lang.Float.NEGATIVE_INFINITY

  /** Positive infinity of type [[scala.Float]]. */
  @deprecated("Use `scala.Float.PositiveInfinity` instead", "2.8.0")
  val POS_INF_FLOAT = java.lang.Float.POSITIVE_INFINITY

  /** The smallest possible value for [[scala.Double]]. */
  @deprecated("Use `scala.Double.MinValue` instead", "2.8.0")
  val MIN_DOUBLE = -java.lang.Double.MAX_VALUE

  /** The smallest difference between two values of [[scala.Double]]. */
  @deprecated("Use `scala.Double.MinPositiveValue` instead", "2.8.0")
  val EPS_DOUBLE = java.lang.Double.MIN_VALUE

  /** The greatest possible value for [[scala.Double]]. */
  @deprecated("Use `scala.Double.MaxValue` instead", "2.8.0")
  val MAX_DOUBLE = java.lang.Double.MAX_VALUE

  /** A value of type [[scala.Double]] that represents no number. */
  @deprecated("Use `scala.Double.NaN` instead", "2.8.0")
  val NaN_DOUBLE = java.lang.Double.NaN

  /** Negative infinity of type [[scala.Double]]. */
  @deprecated("Use `scala.Double.NegativeInfinity` instead", "2.8.0")
  val NEG_INF_DOUBLE = java.lang.Double.NEGATIVE_INFINITY

  /** Positive infinity of type [[scala.Double]]. */
  @deprecated("Use `scala.Double.PositiveInfinity` instead", "2.8.0")
  val POS_INF_DOUBLE = java.lang.Double.POSITIVE_INFINITY
}