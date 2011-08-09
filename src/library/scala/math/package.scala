/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** The package object `scala.math` contains methods for performing basic
  * numeric operations such as elementary exponential, logarithmic, root and
  * trigonometric functions.
  */
package object math extends MathCommon {

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
