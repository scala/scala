/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat;


object Math {
  val MIN_BYTE   = java.lang.Byte.MIN_VALUE
  val MAX_BYTE   = java.lang.Byte.MAX_VALUE
  val MIN_SHORT  = java.lang.Short.MIN_VALUE
  val MAX_SHORT  = java.lang.Short.MAX_VALUE
  val MIN_CHAR   = java.lang.Character.MIN_VALUE
  val MAX_CHAR   = java.lang.Character.MAX_VALUE
  val MIN_INT    = java.lang.Integer.MIN_VALUE
  val MAX_INT    = java.lang.Integer.MAX_VALUE
  val MIN_LONG   = java.lang.Long.MIN_VALUE
  val MAX_LONG   = java.lang.Long.MAX_VALUE

  val MIN_FLOAT  = -java.lang.Float.MAX_VALUE
  val EPS_FLOAT  = java.lang.Float.MIN_VALUE
  val MAX_FLOAT  = java.lang.Float.MAX_VALUE
  val NaN_FLOAT  = java.lang.Float.NaN
  val NEG_INF_FLOAT = java.lang.Float.NEGATIVE_INFINITY
  val POS_INF_FLOAT = java.lang.Float.POSITIVE_INFINITY

  val MIN_DOUBLE = -java.lang.Double.MAX_VALUE
  val EPS_DOUBLE = java.lang.Double.MIN_VALUE
  val MAX_DOUBLE = java.lang.Double.MAX_VALUE
  val NaN_DOUBLE = java.lang.Double.NaN
  val NEG_INF_DOUBLE = java.lang.Double.NEGATIVE_INFINITY
  val POS_INF_DOUBLE = java.lang.Double.POSITIVE_INFINITY

  val E = java.lang.Math.E
  val PI = java.lang.Math.PI

  def min(x: Int, y: Int): Int = java.lang.Math.min(x, y)
  def max(x: Int, y: Int): Int = java.lang.Math.max(x, y)

  def ceil (x: Double): Double = java.lang.Math.ceil(x)
  def floor(x: Double): Double = java.lang.Math.floor(x)
  def log  (x: Double): Double = java.lang.Math.log(x)
  def sqrt (x: Double): Double = java.lang.Math.sqrt(x)
}
