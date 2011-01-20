/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
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

  def isValidByte  = isWhole && (toInt == toByte)
  def isValidShort = isWhole && (toInt == toShort)
  def isValidInt   = isWhole && (toLong == toInt)
  def isValidChar  = isWhole && (toInt >= Char.MinValue && toInt <= Char.MaxValue)

  protected def unifiedPrimitiveHashcode() = {
    val lv = toLong
    if (lv >= Int.MinValue && lv <= Int.MaxValue) lv.toInt
    else lv.##
  }

  /** Should only be called after all known non-primitive
   *  types have been excluded.  This method won't dispatch
   *  anywhere else after checking against the primitives
   *  to avoid infinite recursion between equals and this on
   *  unknown "Number" variants.
   *
   *  Additionally, this should only be called if the numeric
   *  type is happy to be converted to Long, Float, and Double.
   *  If for instance a BigInt much larger than the Long range is
   *  sent here, it will claim equality with whatever Long is left
   *  in its lower 64 bits.  Or a BigDecimal with more precision
   *  than Double can hold: same thing.  There's no way given the
   *  interface available here to prevent this error.
   */
  protected def unifiedPrimitiveEquals(x: Any) = x match {
    case x: Char    => isValidChar && (toInt == x.toInt)
    case x: Byte    => isValidByte && (toByte == x)
    case x: Short   => isValidShort && (toShort == x)
    case x: Int     => isValidInt && (toInt == x)
    case x: Long    => toLong == x
    case x: Float   => toFloat == x
    case x: Double  => toDouble == x
    case _          => false
  }
}
