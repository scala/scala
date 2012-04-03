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
  /** Returns the value of this as a [[scala.Char]]. This may involve
    * rounding or truncation.
    */
  def toChar = intValue.toChar

  /** Returns the value of this as a [[scala.Byte]]. This may involve
    * rounding or truncation.
    */
  def toByte = byteValue

  /** Returns the value of this as a [[scala.Short]]. This may involve
    * rounding or truncation.
    */
  def toShort = shortValue

  /** Returns the value of this as an [[scala.Int]]. This may involve
    * rounding or truncation.
    */
  def toInt = intValue

  /** Returns the value of this as a [[scala.Long]]. This may involve
    * rounding or truncation.
    */
  def toLong = longValue

  /** Returns the value of this as a [[scala.Float]]. This may involve
    * rounding or truncation.
    */
  def toFloat = floatValue

  /** Returns the value of this as a [[scala.Double]]. This may involve
    * rounding or truncation.
    */
  def toDouble = doubleValue

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Byte]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidByte  = isValidInt && (toInt == toByte)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidShort = isValidInt && (toInt == toShort)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidChar  = isValidInt && (toInt == toChar)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidInt   = isValidLong && (toLong == toInt)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Long]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidLong: Boolean

  /** Returns `true` iff this can be represented exactly by [[scala.Float]]; otherwise returns `false`.
    * Special cases:
    *   returns 'true' when this is infinity
    *   returns 'false' when this is NaN.
    */
  def isValidFloat = isValidDouble && (toDouble == toFloat)

  /** Returns `true` iff this can be represented exactly by [[scala.Double]]; otherwise returns `false`.
    * Special cases:
    *   returns 'true' when this is infinity
    *   returns 'false' when this is NaN.
    */
  def isValidDouble: Boolean

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
   */
  protected def unifiedPrimitiveEquals(x: Any) = x match {
    case x: Char    => isValidChar && (toInt == x.toInt)
    case x: Byte    => isValidByte && (toByte == x)
    case x: Short   => isValidShort && (toShort == x)
    case x: Int     => isValidInt && (toInt == x)
    case x: Long    => isValidLong && (toLong == x)
    case x: Float   => isValidFloat && (toFloat == x)
    case x: Double  => isValidDouble && (toDouble == x)
    case _          => false
  }
}
