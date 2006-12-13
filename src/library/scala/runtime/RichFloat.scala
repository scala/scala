/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


final class RichFloat(x: Float) extends Proxy with Ordered[Float] {

  // Proxy.self
  def self: Any = x

  // Ordered[Float].compare
  def compare (y: Float): Int = if (x < y) -1 else if (x > y) 1 else 0

  def min(y: Float) = Math.min(x, y)
  def max(y: Float) = Math.max(x, y)
  def abs: Float = Math.abs(x)

  // isNaN is provided by the implicit conversion to java.lang.Float
  // def isNaN: Boolean = java.lang.Float.isNaN(x)
  def isInfinity: Boolean = java.lang.Float.isInfinite(x)
  def isPosInfinity: Boolean = isInfinity && x > 0.0f
  def isNegInfinity: Boolean = isInfinity && x < 0.0f

}
