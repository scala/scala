/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

import language.implicitConversions

/** `Short`, a 16-bit signed integer (equivalent to Java's `short` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Short` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Short]] => [[scala.runtime.RichShort]]
 *  which provides useful non-primitive operations.
 */
final abstract class Short private extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double

  /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 */
  def unary_~ : Int
  /**
 * Returns this value, unmodified.
 */
  def unary_+ : Int
  /**
 * Returns the negation of this value.
 */
  def unary_- : Int

  def +(x: String): String

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Int): Int
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Long): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Int): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Long): Int
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Int): Int
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Long): Int

  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Byte): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Short): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Char): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Int): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Long): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Float): Boolean
  /**
  * Returns `true` if this value is equal to x, `false` otherwise.
  */
  def ==(x: Double): Boolean

  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Byte): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Short): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Char): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Int): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Long): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Float): Boolean
  /**
  * Returns `true` if this value is not equal to x, `false` otherwise.
  */
  def !=(x: Double): Boolean

  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Byte): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Short): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Char): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Int): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Long): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Float): Boolean
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: Double): Boolean

  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Byte): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Short): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Char): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Int): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Long): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Float): Boolean
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: Double): Boolean

  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Byte): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Short): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Char): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Int): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Long): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Float): Boolean
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: Double): Boolean

  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Byte): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Short): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Char): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Int): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Long): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Float): Boolean
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: Double): Boolean

  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Byte): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Short): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Char): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Int): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Long): Long

  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Byte): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Short): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Char): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Int): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Long): Long

  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Byte): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Short): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Char): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Int): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Long): Long

  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Byte): Int
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Short): Int
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Char): Int
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Int): Int
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Long): Long
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Float): Float
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: Double): Double

  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Byte): Int
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Short): Int
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Char): Int
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Int): Int
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Long): Long
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Float): Float
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: Double): Double

  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Byte): Int
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Short): Int
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Char): Int
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Int): Int
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Long): Long
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Float): Float
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: Double): Double

  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Byte): Int
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Short): Int
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Char): Int
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Int): Int
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Long): Long
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Float): Float
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: Double): Double

  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Byte): Int
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Short): Int
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Char): Int
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Int): Int
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Long): Long
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Float): Float
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: Double): Double

  override def getClass(): Class[Short] = null
}

object Short extends AnyValCompanion {
  /** The smallest value representable as a Short.
   */
  final val MinValue = java.lang.Short.MIN_VALUE

  /** The largest value representable as a Short.
   */
  final val MaxValue = java.lang.Short.MAX_VALUE

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Short to be boxed
   *  @return     a java.lang.Short offering `x` as its underlying value.
   */
  def box(x: Short): java.lang.Short = java.lang.Short.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Short.
   *
   *  @param  x   the java.lang.Short to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Short
   *  @return     the Short resulting from calling shortValue() on `x`
   */
  def unbox(x: java.lang.Object): Short = x.asInstanceOf[java.lang.Short].shortValue()

  /** The String representation of the scala.Short companion object.
   */
  override def toString = "object scala.Short"

  /** Language mandated coercions from Short to "wider" types.
   */
  implicit def short2int(x: Short): Int = x.toInt
  implicit def short2long(x: Short): Long = x.toLong
  implicit def short2float(x: Short): Float = x.toFloat
  implicit def short2double(x: Short): Double = x.toDouble
}

