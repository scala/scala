/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

import java.{ lang => jl }


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

}


object Long extends AnyValCompanion {
  final val MinValue = jl.Long.MIN_VALUE
  final val MaxValue = jl.Long.MAX_VALUE

  def box(x: Long): jl.Long = jl.Long.valueOf(x)
  def unbox(x: jl.Object): Long = x.asInstanceOf[jl.Long].longValue()
  override def toString = "object scala.Long"
}
