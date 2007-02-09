/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


object Math {
  val MIN_BYTE   = System.Byte.MinValue
  val MAX_BYTE   = System.Byte.MaxValue
  val MIN_SHORT  = System.Int16.MinValue
  val MAX_SHORT  = System.Int16.MaxValue
  val MIN_CHAR   = System.Char.MinValue
  val MAX_CHAR   = System.Char.MaxValue
  val MIN_INT    = System.Int32.MinValue
  val MAX_INT    = System.Int32.MaxValue
  val MIN_LONG   = System.Int64.MinValue
  val MAX_LONG   = System.Int64.MaxValue

  val MIN_FLOAT  = System.Single.MinValue
  val EPS_FLOAT  = System.Single.Epsilon
  val MAX_FLOAT  = System.Single.MinValue
  //val NaN_FLOAT  = System.Single.NaN
  //val NEG_INF_FLOAT = System.Double.NegativeInfinity
  //val POS_INF_FLOAT = System.Double.PositiveInfinity

  val MIN_DOUBLE = System.Double.MinValue
  val EPS_DOUBLE = System.Double.Epsilon
  val MAX_DOUBLE = System.Double.MaxValue
  //val NaN_DOUBLE = System.Double.NaN
  //val NEG_INF_DOUBLE = System.Double.NegativeInfinity
  //val POS_INF_DOUBLE = System.Double.PositiveInfinity

  def E: Double = System.Math.E
  def PI: Double = System.Math.PI

  def min(x: Int, y: Int): Int = System.Math.Min(x, y)
  def max(x: Int, y: Int): Int = System.Math.Max(x, y)

  def ceil (x: Double): Double = System.Math.Ceiling(x)
  def floor(x: Double): Double = System.Math.Floor(x)
  def log  (x: Double): Double = System.Math.Log(x)
  def sqrt (x: Double): Double = System.Math.Sqrt(x)
}
