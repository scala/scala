/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Float`, a 32-bit IEEE-754 floating point number (equivalent to Java's `float` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Float` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Float]] => [[scala.runtime.RichFloat]]
 *  which provides useful non-primitive operations.
 */
final class Float extends AnyVal {
  def toByte: Byte = sys.error("stub")
  def toShort: Short = sys.error("stub")
  def toChar: Char = sys.error("stub")
  def toInt: Int = sys.error("stub")
  def toLong: Long = sys.error("stub")
  def toFloat: Float = sys.error("stub")
  def toDouble: Double = sys.error("stub")

  /**
 * @return this value, unmodified
 */
  def unary_+ : Float = sys.error("stub")
  /**
 * @return the negation of this value
 */
  def unary_- : Float = sys.error("stub")

  def +(x: String): String = sys.error("stub")

  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is equal x, `false` otherwise
  */
  def ==(x: Double): Boolean = sys.error("stub")

  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is not equal to x, `false` otherwise
  */
  def !=(x: Double): Boolean = sys.error("stub")

  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than x, `false` otherwise
  */
  def <(x: Double): Boolean = sys.error("stub")

  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is less than or equal to x, `false` otherwise
  */
  def <=(x: Double): Boolean = sys.error("stub")

  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than x, `false` otherwise
  */
  def >(x: Double): Boolean = sys.error("stub")

  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Byte): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Short): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Char): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Int): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Long): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Float): Boolean = sys.error("stub")
  /**
  * @return `true` if this value is greater than or equal to x, `false` otherwise
  */
  def >=(x: Double): Boolean = sys.error("stub")

  /**
  * @return the sum of this value and x
  */
  def +(x: Byte): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Short): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Char): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Int): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Long): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Float): Float = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Double): Double = sys.error("stub")

  /**
  * @return the difference of this value and x
  */
  def -(x: Byte): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Short): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Char): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Int): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Long): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Float): Float = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Double): Double = sys.error("stub")

  /**
  * @return the product of this value and x
  */
  def *(x: Byte): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Short): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Char): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Int): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Long): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Float): Float = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Double): Double = sys.error("stub")

  /**
  * @return the quotient of this value and x
  */
  def /(x: Byte): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Short): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Char): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Int): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Long): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Float): Float = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Double): Double = sys.error("stub")

  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Byte): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Short): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Char): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Int): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Long): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Float): Float = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Double): Double = sys.error("stub")

  override def getClass(): Class[Float] = sys.error("stub")
}

object Float extends AnyValCompanion {
  /** The smallest positive value greater than 0.0f which is
   *  representable as a Float.
   */
  final val MinPositiveValue = java.lang.Float.MIN_VALUE
  final val NaN              = java.lang.Float.NaN
  final val PositiveInfinity = java.lang.Float.POSITIVE_INFINITY
  final val NegativeInfinity = java.lang.Float.NEGATIVE_INFINITY

  @deprecated("use Float.MinPositiveValue instead", "2.9.0")
  final val Epsilon  = MinPositiveValue

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
   *  @param  x   the Float to be boxed
   *  @return     a java.lang.Float offering `x` as its underlying value.
   */
  def box(x: Float): java.lang.Float = java.lang.Float.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Float.
   *
   *  @param  x   the java.lang.Float to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Float
   *  @return     the Float resulting from calling floatValue() on `x`
   */
  def unbox(x: java.lang.Object): Float = x.asInstanceOf[java.lang.Float].floatValue()

  /** The String representation of the scala.Float companion object.
   */
  override def toString = "object scala.Float"
}

