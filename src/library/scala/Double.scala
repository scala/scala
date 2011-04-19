/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Double` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
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

  def unary_+ : Double = sys.error("stub")
  def unary_- : Double = sys.error("stub")

  def +(x: String): String = sys.error("stub")

  def ==(x: Byte): Boolean = sys.error("stub")
  def ==(x: Short): Boolean = sys.error("stub")
  def ==(x: Char): Boolean = sys.error("stub")
  def ==(x: Int): Boolean = sys.error("stub")
  def ==(x: Long): Boolean = sys.error("stub")
  def ==(x: Float): Boolean = sys.error("stub")
  def ==(x: Double): Boolean = sys.error("stub")

  def !=(x: Byte): Boolean = sys.error("stub")
  def !=(x: Short): Boolean = sys.error("stub")
  def !=(x: Char): Boolean = sys.error("stub")
  def !=(x: Int): Boolean = sys.error("stub")
  def !=(x: Long): Boolean = sys.error("stub")
  def !=(x: Float): Boolean = sys.error("stub")
  def !=(x: Double): Boolean = sys.error("stub")

  def <(x: Byte): Boolean = sys.error("stub")
  def <(x: Short): Boolean = sys.error("stub")
  def <(x: Char): Boolean = sys.error("stub")
  def <(x: Int): Boolean = sys.error("stub")
  def <(x: Long): Boolean = sys.error("stub")
  def <(x: Float): Boolean = sys.error("stub")
  def <(x: Double): Boolean = sys.error("stub")

  def <=(x: Byte): Boolean = sys.error("stub")
  def <=(x: Short): Boolean = sys.error("stub")
  def <=(x: Char): Boolean = sys.error("stub")
  def <=(x: Int): Boolean = sys.error("stub")
  def <=(x: Long): Boolean = sys.error("stub")
  def <=(x: Float): Boolean = sys.error("stub")
  def <=(x: Double): Boolean = sys.error("stub")

  def >(x: Byte): Boolean = sys.error("stub")
  def >(x: Short): Boolean = sys.error("stub")
  def >(x: Char): Boolean = sys.error("stub")
  def >(x: Int): Boolean = sys.error("stub")
  def >(x: Long): Boolean = sys.error("stub")
  def >(x: Float): Boolean = sys.error("stub")
  def >(x: Double): Boolean = sys.error("stub")

  def >=(x: Byte): Boolean = sys.error("stub")
  def >=(x: Short): Boolean = sys.error("stub")
  def >=(x: Char): Boolean = sys.error("stub")
  def >=(x: Int): Boolean = sys.error("stub")
  def >=(x: Long): Boolean = sys.error("stub")
  def >=(x: Float): Boolean = sys.error("stub")
  def >=(x: Double): Boolean = sys.error("stub")

  def +(x: Byte): Double = sys.error("stub")
  def +(x: Short): Double = sys.error("stub")
  def +(x: Char): Double = sys.error("stub")
  def +(x: Int): Double = sys.error("stub")
  def +(x: Long): Double = sys.error("stub")
  def +(x: Float): Double = sys.error("stub")
  def +(x: Double): Double = sys.error("stub")

  def -(x: Byte): Double = sys.error("stub")
  def -(x: Short): Double = sys.error("stub")
  def -(x: Char): Double = sys.error("stub")
  def -(x: Int): Double = sys.error("stub")
  def -(x: Long): Double = sys.error("stub")
  def -(x: Float): Double = sys.error("stub")
  def -(x: Double): Double = sys.error("stub")

  def *(x: Byte): Double = sys.error("stub")
  def *(x: Short): Double = sys.error("stub")
  def *(x: Char): Double = sys.error("stub")
  def *(x: Int): Double = sys.error("stub")
  def *(x: Long): Double = sys.error("stub")
  def *(x: Float): Double = sys.error("stub")
  def *(x: Double): Double = sys.error("stub")

  def /(x: Byte): Double = sys.error("stub")
  def /(x: Short): Double = sys.error("stub")
  def /(x: Char): Double = sys.error("stub")
  def /(x: Int): Double = sys.error("stub")
  def /(x: Long): Double = sys.error("stub")
  def /(x: Float): Double = sys.error("stub")
  def /(x: Double): Double = sys.error("stub")

  def %(x: Byte): Double = sys.error("stub")
  def %(x: Short): Double = sys.error("stub")
  def %(x: Char): Double = sys.error("stub")
  def %(x: Int): Double = sys.error("stub")
  def %(x: Long): Double = sys.error("stub")
  def %(x: Float): Double = sys.error("stub")
  def %(x: Double): Double = sys.error("stub")

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

