/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.math

import java.{ lang => jl }

/** Conversions which present a consistent conversion interface
 *  across all the numeric types.
 */
trait ScalaNumericConversions extends ScalaNumber {
  def toChar = intValue.toChar
  def toByte = byteValue
  def toShort = shortValue
  def toInt = intValue
  def toLong = longValue
  def toFloat = floatValue
  def toDouble = doubleValue

  def isValidByte = isWhole && (toByte == toInt)
  def isValidShort = isWhole && (toShort == toInt)
  def isValidInt = isWhole && (toInt == toLong)
  def isValidChar = isWhole && (toInt >= Char.MinValue && toInt <= Char.MaxValue)

  protected def unifiedPrimitiveHashcode() = {
    val lv = toLong
    if (lv >= Int.MinValue && lv <= Int.MaxValue) lv.toInt
    else lv.hashCode
  }

  protected def unifiedPrimitiveEquals(x: Any) = x match {
    case x: Char    => isValidChar && (toInt == x.toInt)
    case x: Byte    => isValidByte && (toByte == x)
    case x: Short   => isValidShort && (toShort == x)
    case x: Int     => isValidInt && (toInt == x)
    case x: Long    => toLong == x    // XXX
    case x: Float   => toFloat == x   // XXX
    case x: Double  => toDouble == x  // XXX
    case x: Number  => this equals x
    case _          => false
  }
}
