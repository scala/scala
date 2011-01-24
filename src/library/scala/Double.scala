/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Sun Jan 23 21:13:38 PST 2011

package scala

import java.{ lang => jl }


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
  final val MinPositiveValue = jl.Double.MIN_VALUE
  final val MinNegativeValue = -jl.Double.MAX_VALUE
  final val NaN              = jl.Double.NaN
  final val PositiveInfinity = jl.Double.POSITIVE_INFINITY
  final val NegativeInfinity = jl.Double.NEGATIVE_INFINITY

  @deprecated("use Double.MinPositiveValue instead")
  final val Epsilon          = MinPositiveValue
  @deprecated("use Double.MinNegativeValue instead")
  final val MinValue = MinNegativeValue
  final val MaxValue = jl.Double.MAX_VALUE

  def box(x: Double): jl.Double = jl.Double.valueOf(x)
  def unbox(x: jl.Object): Double = x.asInstanceOf[jl.Double].doubleValue()
  override def toString = "object scala.Double"
}
