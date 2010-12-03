/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala

/** The object <code>Math</code> contains methods for performing basic numeric
 *  operations such as the elementary exponential, logarithm, square root, and
 *  trigonometric functions.
 */
@deprecated("use scala.math package object instead.\n(Example package object usage: scala.math.Pi )")
object Math extends MathCommon {
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
}