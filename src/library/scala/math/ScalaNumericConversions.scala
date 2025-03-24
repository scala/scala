/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

/** A slightly more specific conversion trait for classes which
 *  extend ScalaNumber (which excludes value classes.)
 */
trait ScalaNumericConversions extends ScalaNumber with ScalaNumericAnyConversions {
  def underlying: Object
}

/** Conversions which present a consistent conversion interface
 *  across all the numeric types, suitable for use in value classes.
 */
trait ScalaNumericAnyConversions extends Any {
  /** @return `'''true'''` if this number has no decimal component, `'''false'''` otherwise. */
  def isWhole: Boolean

  def byteValue: Byte
  def shortValue: Short
  def intValue: Int
  def longValue: Long
  def floatValue: Float
  def doubleValue: Double

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
  def isValidByte  = isWhole && (toInt == toByte)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidShort = isWhole && (toInt == toShort)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidInt   = isWhole && (toLong == toInt)

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidChar  = isWhole && (toInt >= Char.MinValue && toInt <= Char.MaxValue)

  protected def unifiedPrimitiveHashcode = {
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
