/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST
// This auto-generated code can be modified in "project/GenerateAnyVals.scala".
// Afterwards, running "sbt generateSources" regenerates this source file.

package scala

/** `Float`, a 32-bit IEEE-754 floating point number (equivalent to Java's `float` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Float` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Float]] => [[scala.runtime.RichFloat]]
 *  which provides useful non-primitive operations.
 */
final abstract class Float private extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double

  /** Returns this value, unmodified. */
  def unary_+ : Float
  /** Returns the negation of this value. */
  def unary_- : Float

  def +(x: String): String

  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Byte): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Short): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Char): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Int): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Long): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Float): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Double): Boolean

  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Byte): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Short): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Char): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Int): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Long): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Float): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Double): Boolean

  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Byte): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Short): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Char): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Int): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Long): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Float): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Double): Boolean

  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Byte): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Short): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Char): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Int): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Long): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Float): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Double): Boolean

  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Byte): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Short): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Char): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Int): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Long): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Float): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Double): Boolean

  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Byte): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Short): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Char): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Int): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Long): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Float): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Double): Boolean

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Float
  /** Returns the product of this value and `x`. */
  def *(x: Short): Float
  /** Returns the product of this value and `x`. */
  def *(x: Char): Float
  /** Returns the product of this value and `x`. */
  def *(x: Int): Float
  /** Returns the product of this value and `x`. */
  def *(x: Long): Float
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double

  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Float] = ???
}

object Float extends AnyValCompanion {
  /** The smallest positive value greater than 0.0f which is
   *  representable as a Float.
   */
  final val MinPositiveValue = java.lang.Float.MIN_VALUE
  final val NaN              = java.lang.Float.NaN
  final val PositiveInfinity = java.lang.Float.POSITIVE_INFINITY
  final val NegativeInfinity = java.lang.Float.NEGATIVE_INFINITY

  /** The negative number with the greatest (finite) absolute value which is representable
   *  by a Float.  Note that it differs from [[java.lang.Float.MIN_VALUE]], which
   *  is the smallest positive value representable by a Float.  In Scala that number
   *  is called Float.MinPositiveValue.
   */
  final val MinValue = -java.lang.Float.MAX_VALUE

  /** The largest finite positive number representable as a Float. */
  final val MaxValue = java.lang.Float.MAX_VALUE

  /** Transform a value type into a boxed reference type.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToFloat`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the Float to be boxed
   *  @return     a java.lang.Float offering `x` as its underlying value.
   */
  def box(x: Float): java.lang.Float = ???

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Float.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToFloat`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the java.lang.Float to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Float
   *  @return     the Float resulting from calling floatValue() on `x`
   */
  def unbox(x: java.lang.Object): Float = ???

  /** The String representation of the scala.Float companion object. */
  override def toString = "object scala.Float"
  /** Language mandated coercions from Float to "wider" types. */
  import scala.language.implicitConversions
  implicit def float2double(x: Float): Double = x.toDouble
}

