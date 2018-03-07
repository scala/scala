/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package math

import scala.language.implicitConversions
import scala.util.Try

/**
 * @since 2.8
 */
object Numeric {
  @inline def apply[T](implicit num: Numeric[T]): Numeric[T] = num

  trait ExtraImplicits {
    /** These implicits create conversions from a value for which an implicit Numeric
     *  exists to the inner class which creates infix operations.  Once imported, you
     *  can write methods as follows:
     *  {{{
     *  def plus[T: Numeric](x: T, y: T) = x + y
     *  }}}
     */
    implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): Numeric[T]#Ops = new num.Ops(x)
  }
  object Implicits extends ExtraImplicits { }

  trait BigIntIsIntegral extends Integral[BigInt] {
    def plus(x: BigInt, y: BigInt): BigInt = x + y
    def minus(x: BigInt, y: BigInt): BigInt = x - y
    def times(x: BigInt, y: BigInt): BigInt = x * y
    def quot(x: BigInt, y: BigInt): BigInt = x / y
    def rem(x: BigInt, y: BigInt): BigInt = x % y
    def negate(x: BigInt): BigInt = -x
    def fromInt(x: Int): BigInt = BigInt(x)
    def parseString(str: String): Option[BigInt] = Try(BigInt(str)).toOption
    def toInt(x: BigInt): Int = x.intValue
    def toLong(x: BigInt): Long = x.longValue
    def toFloat(x: BigInt): Float = x.floatValue
    def toDouble(x: BigInt): Double = x.doubleValue
  }
  implicit object BigIntIsIntegral extends BigIntIsIntegral with Ordering.BigIntOrdering

  trait IntIsIntegral extends Integral[Int] {
    def plus(x: Int, y: Int): Int = x + y
    def minus(x: Int, y: Int): Int = x - y
    def times(x: Int, y: Int): Int = x * y
    def quot(x: Int, y: Int): Int = x / y
    def rem(x: Int, y: Int): Int = x % y
    def negate(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def parseString(str: String): Option[Int] = Try(str.toInt).toOption
    def toInt(x: Int): Int = x
    def toLong(x: Int): Long = x.toLong
    def toFloat(x: Int): Float = x.toFloat
    def toDouble(x: Int): Double = x.toDouble
  }
  implicit object IntIsIntegral extends IntIsIntegral with Ordering.IntOrdering

  trait ShortIsIntegral extends Integral[Short] {
    def plus(x: Short, y: Short): Short = (x + y).toShort
    def minus(x: Short, y: Short): Short = (x - y).toShort
    def times(x: Short, y: Short): Short = (x * y).toShort
    def quot(x: Short, y: Short): Short = (x / y).toShort
    def rem(x: Short, y: Short): Short = (x % y).toShort
    def negate(x: Short): Short = (-x).toShort
    def fromInt(x: Int): Short = x.toShort
    def parseString(str: String): Option[Short] = Try(str.toShort).toOption
    def toInt(x: Short): Int = x.toInt
    def toLong(x: Short): Long = x.toLong
    def toFloat(x: Short): Float = x.toFloat
    def toDouble(x: Short): Double = x.toDouble
  }
  implicit object ShortIsIntegral extends ShortIsIntegral with Ordering.ShortOrdering

  trait ByteIsIntegral extends Integral[Byte] {
    def plus(x: Byte, y: Byte): Byte = (x + y).toByte
    def minus(x: Byte, y: Byte): Byte = (x - y).toByte
    def times(x: Byte, y: Byte): Byte = (x * y).toByte
    def quot(x: Byte, y: Byte): Byte = (x / y).toByte
    def rem(x: Byte, y: Byte): Byte = (x % y).toByte
    def negate(x: Byte): Byte = (-x).toByte
    def fromInt(x: Int): Byte = x.toByte
    def parseString(str: String): Option[Byte] = Try(str.toByte).toOption
    def toInt(x: Byte): Int = x.toInt
    def toLong(x: Byte): Long = x.toLong
    def toFloat(x: Byte): Float = x.toFloat
    def toDouble(x: Byte): Double = x.toDouble
  }
  implicit object ByteIsIntegral extends ByteIsIntegral with Ordering.ByteOrdering

  trait CharIsIntegral extends Integral[Char] {
    def plus(x: Char, y: Char): Char = (x + y).toChar
    def minus(x: Char, y: Char): Char = (x - y).toChar
    def times(x: Char, y: Char): Char = (x * y).toChar
    def quot(x: Char, y: Char): Char = (x / y).toChar
    def rem(x: Char, y: Char): Char = (x % y).toChar
    def negate(x: Char): Char = (-x).toChar
    def fromInt(x: Int): Char = x.toChar
    def parseString(str: String): Option[Char] = Try(str.toInt.toChar).toOption
    def toInt(x: Char): Int = x.toInt
    def toLong(x: Char): Long = x.toLong
    def toFloat(x: Char): Float = x.toFloat
    def toDouble(x: Char): Double = x.toDouble
  }
  implicit object CharIsIntegral extends CharIsIntegral with Ordering.CharOrdering

  trait LongIsIntegral extends Integral[Long] {
    def plus(x: Long, y: Long): Long = x + y
    def minus(x: Long, y: Long): Long = x - y
    def times(x: Long, y: Long): Long = x * y
    def quot(x: Long, y: Long): Long = x / y
    def rem(x: Long, y: Long): Long = x % y
    def negate(x: Long): Long = -x
    def fromInt(x: Int): Long = x.toLong
    def parseString(str: String): Option[Long] = Try(str.toLong).toOption
    def toInt(x: Long): Int = x.toInt
    def toLong(x: Long): Long = x
    def toFloat(x: Long): Float = x.toFloat
    def toDouble(x: Long): Double = x.toDouble
  }
  implicit object LongIsIntegral extends LongIsIntegral with Ordering.LongOrdering

  trait FloatIsConflicted extends Numeric[Float] {
    def plus(x: Float, y: Float): Float = x + y
    def minus(x: Float, y: Float): Float = x - y
    def times(x: Float, y: Float): Float = x * y
    def negate(x: Float): Float = -x
    def fromInt(x: Int): Float = x.toFloat
    def parseString(str: String): Option[Float] = Try(str.toFloat).toOption
    def toInt(x: Float): Int = x.toInt
    def toLong(x: Float): Long = x.toLong
    def toFloat(x: Float): Float = x
    def toDouble(x: Float): Double = x.toDouble
    // logic in Numeric base trait mishandles abs(-0.0f)
    override def abs(x: Float): Float = math.abs(x)

    override protected def deprecatedAsOrdering: Ordering[Float] = DeprecatedFloatOrdering
  }
  trait FloatIsFractional extends FloatIsConflicted with Fractional[Float] {
    def div(x: Float, y: Float): Float = x / y
  }
  trait FloatAsIfIntegral extends FloatIsConflicted with Integral[Float] {
    def quot(x: Float, y: Float): Float = (BigDecimal(x) quot BigDecimal(y)).floatValue
    def rem(x: Float, y: Float): Float = (BigDecimal(x) remainder BigDecimal(y)).floatValue
  }
  implicit object FloatIsFractional extends FloatIsFractional with PartialOrdering.FloatPartialOrdering
  object FloatAsIfIntegral extends FloatAsIfIntegral with PartialOrdering.Float.AsUnsafeTotalOrdering

  trait DoubleIsConflicted extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def minus(x: Double, y: Double): Double = x - y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def fromInt(x: Int): Double = x.toDouble
    def parseString(str: String): Option[Double] = Try(str.toDouble).toOption
    def toInt(x: Double): Int = x.toInt
    def toLong(x: Double): Long = x.toLong
    def toFloat(x: Double): Float = x.toFloat
    def toDouble(x: Double): Double = x
    // logic in Numeric base trait mishandles abs(-0.0)
    override def abs(x: Double): Double = math.abs(x)

    override protected def deprecatedAsOrdering: Ordering[Double] = DeprecatedDoubleOrdering
  }
  trait DoubleIsFractional extends DoubleIsConflicted with Fractional[Double] {
    def div(x: Double, y: Double): Double = x / y
  }
  trait DoubleAsIfIntegral extends DoubleIsConflicted with Integral[Double] {
    def quot(x: Double, y: Double): Double = (BigDecimal(x) quot BigDecimal(y)).doubleValue
    def rem(x: Double, y: Double): Double = (BigDecimal(x) remainder BigDecimal(y)).doubleValue
  }

  trait BigDecimalIsConflicted extends Numeric[BigDecimal] {
    def plus(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
    def minus(x: BigDecimal, y: BigDecimal): BigDecimal = x - y
    def times(x: BigDecimal, y: BigDecimal): BigDecimal = x * y
    def negate(x: BigDecimal): BigDecimal = -x
    def fromInt(x: Int): BigDecimal = BigDecimal(x)
    def parseString(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
    def toInt(x: BigDecimal): Int = x.intValue
    def toLong(x: BigDecimal): Long = x.longValue
    def toFloat(x: BigDecimal): Float = x.floatValue
    def toDouble(x: BigDecimal): Double = x.doubleValue
  }

  trait BigDecimalIsFractional extends BigDecimalIsConflicted with Fractional[BigDecimal] {
    def div(x: BigDecimal, y: BigDecimal): BigDecimal = x / y
  }
  trait BigDecimalAsIfIntegral extends BigDecimalIsConflicted with Integral[BigDecimal] {
    def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x quot y
    def rem(x: BigDecimal, y: BigDecimal): BigDecimal = x remainder y
  }

  // For Double and BigDecimal we offer implicit Fractional objects, but also one
  // which acts like an Integral type, which is useful in NumericRange.
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional with Ordering.BigDecimalOrdering
  object BigDecimalAsIfIntegral extends BigDecimalAsIfIntegral with Ordering.BigDecimalOrdering

  implicit object DoubleIsFractional extends DoubleIsFractional with PartialOrdering.DoublePartialOrdering
  object DoubleAsIfIntegral extends DoubleAsIfIntegral with PartialOrdering.Double.AsUnsafeTotalOrdering

  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  implicit def numericAsOrdering[T](numeric: Numeric[T]): Ordering[T] = numeric.deprecatedAsOrdering

  /* Deprecated orderings for backwards compatibility */

  private trait DeprecatedFloatOrdering extends Ordering.FloatOrdering {
    override def lteq(x: Float, y: Float): Boolean = x <= y
    override def gteq(x: Float, y: Float): Boolean = x >= y
    override def lt(x: Float, y: Float): Boolean = x < y
    override def gt(x: Float, y: Float): Boolean = x > y
    override def equiv(x: Float, y: Float): Boolean = x == y
    override def max(x: Float, y: Float): Float = math.max(x, y)
    override def min(x: Float, y: Float): Float = math.min(x, y)
  }
  private object DeprecatedFloatOrdering extends DeprecatedFloatOrdering

  private trait DeprecatedDoubleOrdering extends Ordering.DoubleOrdering {
    override def lteq(x: Double, y: Double): Boolean = x <= y
    override def gteq(x: Double, y: Double): Boolean = x >= y
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def equiv(x: Double, y: Double): Boolean = x == y
    override def max(x: Double, y: Double): Double = math.max(x, y)
    override def min(x: Double, y: Double): Double = math.min(x, y)
  }
  private object DeprecatedDoubleOrdering extends DeprecatedDoubleOrdering
}

trait Numeric[T] extends PartialOrdering[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def fromInt(x: Int): T
  def parseString(str: String): Option[T]
  def toInt(x: T): Int
  def toLong(x: T): Long
  def toFloat(x: T): Float
  def toDouble(x: T): Double

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0

  class Ops(lhs: T) {
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
    def unary_-() = negate(lhs)
    def abs(): T = Numeric.this.abs(lhs)
    def signum(): Int = Numeric.this.signum(lhs)
    def toInt(): Int = Numeric.this.toInt(lhs)
    def toLong(): Long = Numeric.this.toLong(lhs)
    def toFloat(): Float = Numeric.this.toFloat(lhs)
    def toDouble(): Double = Numeric.this.toDouble(lhs)
  }
  implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)

  @deprecated("It doesn't make sense to reverse a Numeric", "2.13.0")
  override def reverse: PartialOrdering[T] = super.reverse

  /** Used for deprecated conversion to [[scala.math.Ordering `Ordering`]].
    *
    * Override if you wish to support exact previous `Ordering` semantics.
    *
    * This method is only expected to exist for two deprecation cycles.
    */
  // TODO: deprecate in 2.14
  protected def deprecatedAsOrdering: Ordering[T] = {
    if (this.isInstanceOf[Ordering[_]]) this.asInstanceOf[Ordering[T]]
    else new AnyRef with AsUnsafeTotalOrdering
  }

  /* deprecated methods previously inherited from Ordering */
  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  def compare(x: T, y: T): Int = deprecatedAsOrdering.compare(x, y)
  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  def max(x: T, y: T): T = deprecatedAsOrdering.max(x, y)
  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  def min(x: T, y: T): T = deprecatedAsOrdering.min(x, y)
  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  def on[U](f: U => T): Ordering[U] = deprecatedAsOrdering.on(f)
  @deprecated("Numeric no longer extends Ordering, as it is not always consistent", "2.13.0")
  implicit def mkOrderingOps(lhs: T): Ordering[T]#Ops = deprecatedAsOrdering.mkOrderingOps(lhs)
}
