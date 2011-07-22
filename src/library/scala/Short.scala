/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Short` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
 *
 *  There is an implicit conversion from [[scala.Short]] => [[scala.runtime.RichShort]]
 *  which provides useful non-primitive operations.
 */
final class Short extends AnyVal {
  def toByte: Byte = sys.error("stub")
  def toShort: Short = sys.error("stub")
  def toChar: Char = sys.error("stub")
  def toInt: Int = sys.error("stub")
  def toLong: Long = sys.error("stub")
  def toFloat: Float = sys.error("stub")
  def toDouble: Double = sys.error("stub")

  def unary_+ : Int = sys.error("stub")
  def unary_- : Int = sys.error("stub")
  def unary_~ : Int = sys.error("stub")

  def +(x: String): String = sys.error("stub")

  def <<(x: Int): Int = sys.error("stub")
  def <<(x: Long): Int = sys.error("stub")
  def >>>(x: Int): Int = sys.error("stub")
  def >>>(x: Long): Int = sys.error("stub")
  def >>(x: Int): Int = sys.error("stub")
  def >>(x: Long): Int = sys.error("stub")

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

  def |(x: Byte): Int = sys.error("stub")
  def |(x: Short): Int = sys.error("stub")
  def |(x: Char): Int = sys.error("stub")
  def |(x: Int): Int = sys.error("stub")
  def |(x: Long): Long = sys.error("stub")

  def &(x: Byte): Int = sys.error("stub")
  def &(x: Short): Int = sys.error("stub")
  def &(x: Char): Int = sys.error("stub")
  def &(x: Int): Int = sys.error("stub")
  def &(x: Long): Long = sys.error("stub")

  def ^(x: Byte): Int = sys.error("stub")
  def ^(x: Short): Int = sys.error("stub")
  def ^(x: Char): Int = sys.error("stub")
  def ^(x: Int): Int = sys.error("stub")
  def ^(x: Long): Long = sys.error("stub")

  def +(x: Byte): Int = sys.error("stub")
  def +(x: Short): Int = sys.error("stub")
  def +(x: Char): Int = sys.error("stub")
  def +(x: Int): Int = sys.error("stub")
  def +(x: Long): Long = sys.error("stub")
  def +(x: Float): Float = sys.error("stub")
  def +(x: Double): Double = sys.error("stub")

  def -(x: Byte): Int = sys.error("stub")
  def -(x: Short): Int = sys.error("stub")
  def -(x: Char): Int = sys.error("stub")
  def -(x: Int): Int = sys.error("stub")
  def -(x: Long): Long = sys.error("stub")
  def -(x: Float): Float = sys.error("stub")
  def -(x: Double): Double = sys.error("stub")

  def *(x: Byte): Int = sys.error("stub")
  def *(x: Short): Int = sys.error("stub")
  def *(x: Char): Int = sys.error("stub")
  def *(x: Int): Int = sys.error("stub")
  def *(x: Long): Long = sys.error("stub")
  def *(x: Float): Float = sys.error("stub")
  def *(x: Double): Double = sys.error("stub")

  def /(x: Byte): Int = sys.error("stub")
  def /(x: Short): Int = sys.error("stub")
  def /(x: Char): Int = sys.error("stub")
  def /(x: Int): Int = sys.error("stub")
  def /(x: Long): Long = sys.error("stub")
  def /(x: Float): Float = sys.error("stub")
  def /(x: Double): Double = sys.error("stub")

  def %(x: Byte): Int = sys.error("stub")
  def %(x: Short): Int = sys.error("stub")
  def %(x: Char): Int = sys.error("stub")
  def %(x: Int): Int = sys.error("stub")
  def %(x: Long): Long = sys.error("stub")
  def %(x: Float): Float = sys.error("stub")
  def %(x: Double): Double = sys.error("stub")

  def getClass(): Class[Short] = sys.error("stub")
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
}

