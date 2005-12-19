/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat;

object Math {
  val MIN_BYTE   = System.SByte.MinValue;
  val MAX_BYTE   = System.SByte.MaxValue;
  val MIN_SHORT  = System.Int16.MinValue;
  val MAX_SHORT  = System.Int16.MaxValue;
  val MIN_CHAR   = System.Char.MinValue;
  val MAX_CHAR   = System.Char.MaxValue;
  val MIN_INT    = System.Int32.MinValue;
  val MAX_INT    = System.Int32.MaxValue;
  val MIN_LONG   = System.Int64.MinValue;
  val MAX_LONG   = System.Int64.MaxValue;
  val MIN_FLOAT  = System.Single.MinValue;
  val MAX_FLOAT  = System.Single.MaxValue;
  val MIN_DOUBLE = System.Double.MinValue;
  val MAX_DOUBLE = System.Double.MaxValue;

  def max(x: Int, y: Int): Int = System.Math.Max(x, y);

  def ceil (x: Double): Double = System.Math.Ceiling(x);
  def floor(x: Double): Double = System.Math.Floor(x);
  def log  (x: Double): Double = System.Math.Log(x);
  def sqrt (x: Double): Double = System.Math.Sqrt(x);
}
