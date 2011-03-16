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


final class Char extends AnyVal {
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


object Char extends AnyValCompanion {
  final val MinValue = jl.Character.MIN_VALUE
  final val MaxValue = jl.Character.MAX_VALUE

  def box(x: Char): jl.Character = jl.Character.valueOf(x)
  def unbox(x: jl.Object): Char = x.asInstanceOf[jl.Character].charValue()
  override def toString = "object scala.Char"
}
