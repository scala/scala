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
  val MAX_INT = java.lang.Integer.MAX_VALUE;
  val MIN_INT = java.lang.Integer.MIN_VALUE;

  def log(x: Double): Double = Math.log(x);
  def max(x: Int, y: Int): Int = Math.max(x, y);
  def sqrt(x: Double): Double = Math.sqrt(s);
}
