/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Long` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
 *
 *  There is an implicit conversion from [[scala.Long]] => [[scala.runtime.RichLong]]
 *  which provides useful non-primitive operations.
 */
final class Long extends AnyVal {
  def toByte: Byte = sys.error("stub")
  def toShort: Short = sys.error("stub")
  def toChar: Char = sys.error("stub")
  def toInt: Int = sys.error("stub")
  def toLong: Long = sys.error("stub")
  def toFloat: Float = sys.error("stub")
  def toDouble: Double = sys.error("stub")

  def unary_+ : Long = sys.error("stub")
  def unary_- : Long = sys.error("stub")
  def unary_~ : Long = sys.error("stub")

  def +(x: String): String = sys.error("stub")

  def <<(x: Int): Long = sys.error("stub")
  def <<(x: Long): Long = sys.error("stub")
  def >>>(x: Int): Long = sys.error("stub")
  def >>>(x: Long): Long = sys.error("stub")
  def >>(x: Int): Long = sys.error("stub")
  def >>(x: Long): Long = sys.error("stub")

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

  def |(x: Byte): Long = sys.error("stub")
  def |(x: Short): Long = sys.error("stub")
  def |(x: Char): Long = sys.error("stub")
  def |(x: Int): Long = sys.error("stub")
  def |(x: Long): Long = sys.error("stub")

  def &(x: Byte): Long = sys.error("stub")
  def &(x: Short): Long = sys.error("stub")
  def &(x: Char): Long = sys.error("stub")
  def &(x: Int): Long = sys.error("stub")
  def &(x: Long): Long = sys.error("stub")

  def ^(x: Byte): Long = sys.error("stub")
  def ^(x: Short): Long = sys.error("stub")
  def ^(x: Char): Long = sys.error("stub")
  def ^(x: Int): Long = sys.error("stub")
  def ^(x: Long): Long = sys.error("stub")

  def +(x: Byte): Long = sys.error("stub")
  def +(x: Short): Long = sys.error("stub")
  def +(x: Char): Long = sys.error("stub")
  def +(x: Int): Long = sys.error("stub")
  def +(x: Long): Long = sys.error("stub")
  def +(x: Float): Float = sys.error("stub")
  def +(x: Double): Double = sys.error("stub")

  def -(x: Byte): Long = sys.error("stub")
  def -(x: Short): Long = sys.error("stub")
  def -(x: Char): Long = sys.error("stub")
  def -(x: Int): Long = sys.error("stub")
  def -(x: Long): Long = sys.error("stub")
  def -(x: Float): Float = sys.error("stub")
  def -(x: Double): Double = sys.error("stub")

  def *(x: Byte): Long = sys.error("stub")
  def *(x: Short): Long = sys.error("stub")
  def *(x: Char): Long = sys.error("stub")
  def *(x: Int): Long = sys.error("stub")
  def *(x: Long): Long = sys.error("stub")
  def *(x: Float): Float = sys.error("stub")
  def *(x: Double): Double = sys.error("stub")

  def /(x: Byte): Long = sys.error("stub")
  def /(x: Short): Long = sys.error("stub")
  def /(x: Char): Long = sys.error("stub")
  def /(x: Int): Long = sys.error("stub")
  def /(x: Long): Long = sys.error("stub")
  def /(x: Float): Float = sys.error("stub")
  def /(x: Double): Double = sys.error("stub")

  def %(x: Byte): Long = sys.error("stub")
  def %(x: Short): Long = sys.error("stub")
  def %(x: Char): Long = sys.error("stub")
  def %(x: Int): Long = sys.error("stub")
  def %(x: Long): Long = sys.error("stub")
  def %(x: Float): Float = sys.error("stub")
  def %(x: Double): Double = sys.error("stub")

  def getClass(): Class[Long] = sys.error("stub")
}

object Long extends AnyValCompanion {
  /** The smallest value representable as a Long.
   */
  final val MinValue = java.lang.Long.MIN_VALUE

  /** The largest value representable as a Long.
   */
  final val MaxValue = java.lang.Long.MAX_VALUE

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Long to be boxed
   *  @return     a java.lang.Long offering `x` as its underlying value.
   */
  def box(x: Long): java.lang.Long = java.lang.Long.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Long.
   *
   *  @param  x   the java.lang.Long to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Long
   *  @return     the Long resulting from calling longValue() on `x`
   */
  def unbox(x: java.lang.Object): Long = x.asInstanceOf[java.lang.Long].longValue()

  /** The String representation of the scala.Long companion object.
   */
  override def toString = "object scala.Long"
}

