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

  val E = System.Math.E
  val Pi = System.Math.PI

  //def random: Double = System.Math.random()

  def sin(x: Double): Double = System.Math.Sin(x)
  def cos(x: Double): Double = System.Math.Cos(x)
  def tan(x: Double): Double = System.Math.Tan(x)
  def asin(x: Double): Double = System.Math.Asin(x)
  def acos(x: Double): Double = System.Math.Acos(x)
  def atan(x: Double): Double = System.Math.Atan(x)
  //def toRadians(x: Double): Double = System.Math.toRadians(x)
  //def toDegrees(x: Double): Double = System.Math.toDegrees(x)
  def exp(x: Double): Double = System.Math.Exp(x)
  def log(x: Double): Double = System.Math.Log(x)
  def sqrt(x: Double): Double = System.Math.Sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = System.Math.IEEERemainder(x, y)
  def ceil(x: Double): Double = System.Math.Ceiling(x)
  def floor(x: Double): Double = System.Math.Floor(x)
  //def rint(x: Double): Double = System.Math.rint(x)
  def atan2(x: Double, y: Double): Double = System.Math.Atan2(x, y)
  def pow(x: Double, y: Double): Double = System.Math.Pow(x, y)
  //def round(x: Float): Int = System.Math.Round(x).toSingle
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

  // from Java 1.5
//   def log10(x: Double): Double = java.lang.Math.log10(x)
//   def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

//   def ulp(x: Double): Double = java.lang.Math.ulp(x)
//   def ulp(x: Float): Float = java.lang.Math.ulp(x)
//   def signum(x: Double): Double = java.lang.Math.signum(x)
//   def signum(x: Float): Float = java.lang.Math.signum(x)
//   def sinh(x: Double): Double = java.lang.Math.sinh(x)
//   def cosh(x: Double): Double = java.lang.Math.cosh(x)
//   def tanh(x: Double):Double = java.lang.Math.tanh(x)
//   def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)
//   def expm1(x: Double): Double = java.lang.Math.expm1(x)
//   def log1p(x: Double): Double = java.lang.Math.log1p(x)

}
