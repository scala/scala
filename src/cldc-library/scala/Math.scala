/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** The object <code>Math</code> contains methods for performing basic numeric
 *  operations such as the elementary exponential, logarithm, square root, and
 *  trigonometric functions.
 */
object Math {

  /** The smalles possible value for scala.Byte. */
  val MIN_BYTE   = java.lang.Byte.MIN_VALUE
  /** The greatest possible value for scala.Byte. */
  val MAX_BYTE   = java.lang.Byte.MAX_VALUE

  /** The smalles possible value for scala.Short. */
  val MIN_SHORT  = java.lang.Short.MIN_VALUE
  /** The greatest possible value for scala.Short. */
  val MAX_SHORT  = java.lang.Short.MAX_VALUE

  /** The smalles possible value for scala.Char. */
  val MIN_CHAR   = java.lang.Character.MIN_VALUE
  /** The greatest possible value for scala.Char. */
  val MAX_CHAR   = java.lang.Character.MAX_VALUE

  /** The smalles possible value for scala.Int. */
  val MIN_INT    = java.lang.Integer.MIN_VALUE
  /** The greatest possible value for scala.Int. */
  val MAX_INT    = java.lang.Integer.MAX_VALUE

  /** The smalles possible value for scala.Long. */
  val MIN_LONG   = java.lang.Long.MIN_VALUE
  /** The greatest possible value for scala.Long. */
  val MAX_LONG   = java.lang.Long.MAX_VALUE


  def abs(x: Int): Int = java.lang.Math.abs(x)
  def abs(x: Long): Long = java.lang.Math.abs(x)

  def max(x: Int, y: Int): Int = java.lang.Math.max(x, y)
  def max(x: Long, y: Long): Long = java.lang.Math.max(x, y)

  def min(x: Int, y: Int): Int = java.lang.Math.min(x, y)
  def min(x: Long, y: Long): Long  = java.lang.Math.min(x, y)

  def sqrt(x: Int): Int = runtime.SquareRoot.accurateSqrt(x)
}
