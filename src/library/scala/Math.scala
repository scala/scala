/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

object Math {

  val E = java.lang.Math.E
  val Pi = java.lang.Math.PI

  def random: Double = java.lang.Math.random()

  def sin(x: Double): Double = java.lang.Math.sin(x)
  def cos(x: Double): Double = java.lang.Math.cos(x)
  def tan(x: Double): Double = java.lang.Math.tan(x)
  def asin(x: Double): Double = java.lang.Math.asin(x)
  def acos(x: Double): Double = java.lang.Math.acos(x)
  def atan(x: Double): Double = java.lang.Math.atan(x)
  def toRadians(x: Double): Double = java.lang.Math.toRadians(x)
  def toDegrees(x: Double): Double = java.lang.Math.toDegrees(x)
  def exp(x: Double): Double = java.lang.Math.exp(x)
  def log(x: Double): Double = java.lang.Math.log(x)
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)
  def ceil(x: Double): Double = java.lang.Math.ceil(x)
  def floor(x: Double): Double = java.lang.Math.floor(x)
  def rint(x: Double): Double = java.lang.Math.rint(x)
  def atan2(x: Double, y: Double): Double = java.lang.Math.atan2(x, y)
  def pow(x: Double, y: Double): Double = java.lang.Math.pow(x, y)
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

  // from Java 1.5
  def log10(x: Double): Double = java.lang.Math.log10(x)
  def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

  def ulp(x: Double): Double = java.lang.Math.ulp(x)
  def ulp(x: Float): Float = java.lang.Math.ulp(x)
  def signum(x: Double): Double = java.lang.Math.signum(x)
  def signum(x: Float): Float = java.lang.Math.signum(x)
  def sinh(x: Double): Double = java.lang.Math.sinh(x)
  def cosh(x: Double): Double = java.lang.Math.cosh(x)
  def tanh(x: Double):Double = java.lang.Math.tanh(x)
  def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)
  def expm1(x: Double): Double = java.lang.Math.expm1(x)
  def log1p(x: Double): Double = java.lang.Math.log1p(x)

}
