/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** The package object `scala.math` contains methods for performing basic
 *  numeric operations such as the elementary exponential, logarithm,
 *  square root, and trigonometric functions.
 */

package object math extends MathCommon {
  // These are new in 2.8, so they don't belong in the deprecated scala.Math.

  def log10(x: Double): Double = java.lang.Math.log10(x)
  def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

  def ulp(x: Double): Double = java.lang.Math.ulp(x)
  def ulp(x: Float): Float = java.lang.Math.ulp(x)
  def sinh(x: Double): Double = java.lang.Math.sinh(x)
  def cosh(x: Double): Double = java.lang.Math.cosh(x)
  def tanh(x: Double):Double = java.lang.Math.tanh(x)
  def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)
  def expm1(x: Double): Double = java.lang.Math.expm1(x)
  def log1p(x: Double): Double = java.lang.Math.log1p(x)
}
