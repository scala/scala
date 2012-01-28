/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Double`, a 64-bit IEEE-754 floating point number (equivalent to Java's `double` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Double` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Double]] => [[scala.runtime.RichDouble]]
 *  which provides useful non-primitive operations.
 */
final class Double extends AnyVal {
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
  def unary_+ : Double = sys.error("stub")
  /**
 * @return the negation of this value
 */
  def unary_- : Double = sys.error("stub")

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
  def +(x: Byte): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Short): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Char): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Int): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Long): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Float): Double = sys.error("stub")
  /**
  * @return the sum of this value and x
  */
  def +(x: Double): Double = sys.error("stub")

  /**
  * @return the difference of this value and x
  */
  def -(x: Byte): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Short): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Char): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Int): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Long): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Float): Double = sys.error("stub")
  /**
  * @return the difference of this value and x
  */
  def -(x: Double): Double = sys.error("stub")

  /**
  * @return the product of this value and x
  */
  def *(x: Byte): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Short): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Char): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Int): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Long): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Float): Double = sys.error("stub")
  /**
  * @return the product of this value and x
  */
  def *(x: Double): Double = sys.error("stub")

  /**
  * @return the quotient of this value and x
  */
  def /(x: Byte): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Short): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Char): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Int): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Long): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Float): Double = sys.error("stub")
  /**
  * @return the quotient of this value and x
  */
  def /(x: Double): Double = sys.error("stub")

  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Byte): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Short): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Char): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Int): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Long): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Float): Double = sys.error("stub")
  /**
  * @return the remainder of the division of this value by x
  */
  def %(x: Double): Double = sys.error("stub")

  override def getClass(): Class[Double] = sys.error("stub")
}

object Double extends AnyValCompanion {
  /** The smallest positive value greater than 0.0d which is
   *  representable as a Double.
   */
  final val MinPositiveValue = java.lang.Double.MIN_VALUE
  final val NaN              = java.lang.Double.NaN
  final val PositiveInfinity = java.lang.Double.POSITIVE_INFINITY
  final val NegativeInfinity = java.lang.Double.NEGATIVE_INFINITY

  @deprecated("use Double.MinPositiveValue instead", "2.9.0")
  final val Epsilon  = MinPositiveValue

  /** The negative number with the greatest (finite) absolute value which is representable
   *  by a Double.  Note that it differs from [[java.lang.Double.MIN_VALUE]], which
   *  is the smallest positive value representable by a Double.  In Scala that number
   *  is called Double.MinPositiveValue.
   */
  final val MinValue = -java.lang.Double.MAX_VALUE

  /** The largest finite positive number representable as a Double. */
  final val MaxValue = java.lang.Double.MAX_VALUE

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Double to be boxed
   *  @return     a java.lang.Double offering `x` as its underlying value.
   */
  def box(x: Double): java.lang.Double = java.lang.Double.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Double.
   *
   *  @param  x   the java.lang.Double to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Double
   *  @return     the Double resulting from calling doubleValue() on `x`
   */
  def unbox(x: java.lang.Object): Double = x.asInstanceOf[java.lang.Double].doubleValue()

  /** The String representation of the scala.Double companion object.
   */
  override def toString = "object scala.Double"
}

