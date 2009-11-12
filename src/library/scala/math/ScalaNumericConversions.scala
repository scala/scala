/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.math

/** Conversions which present a consistent conversion interface
 *  across all the numeric types.
 */
trait ScalaNumericConversions extends java.lang.Number {
  def toChar = intValue.toChar
  def toByte = byteValue
  def toShort = shortValue
  def toInt = intValue
  def toLong = longValue
  def toFloat = floatValue
  def toDouble = doubleValue
}
