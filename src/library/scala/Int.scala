/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Int` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
 *
 *  There is an implicit conversion from [[scala.Int]] => [[scala.runtime.RichInt]]
 *  which provides useful non-primitive operations.
 */
final class Int extends AnyVal {
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

}

object Int extends AnyValCompanion {
  /** The smallest value representable as a Int.
   */
  final val MinValue = java.lang.Integer.MIN_VALUE

  /** The largest value representable as a Int.
   */
  final val MaxValue = java.lang.Integer.MAX_VALUE

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Int to be boxed
   *  @return     a java.lang.Integer offering `x` as its underlying value.
   */
  def box(x: Int): java.lang.Integer = java.lang.Integer.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Integer.
   *
   *  @param  x   the java.lang.Integer to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Integer
   *  @return     the Int resulting from calling intValue() on `x`
   */
  def unbox(x: java.lang.Object): Int = x.asInstanceOf[java.lang.Integer].intValue()

  /** The String representation of the scala.Int companion object.
   */
  override def toString = "object scala.Int"
}

