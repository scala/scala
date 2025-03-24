/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

import scala.collection.StringParsers
import scala.language.implicitConversions
import scala.util.Try

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
    implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): Numeric[T]#NumericOps = new num.NumericOps(x)
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
    def parseString(str: String): Option[Int] = StringParsers.parseInt(str)
    def toInt(x: Int): Int = x
    def toLong(x: Int): Long = x.toLong
    def toFloat(x: Int): Float = x.toFloat
    def toDouble(x: Int): Double = x.toDouble
    override def signum(x: Int): Int = math.signum(x)
    override def sign(x: Int): Int = math.signum(x)
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
    def parseString(str: String): Option[Short] = StringParsers.parseShort(str)
    def toInt(x: Short): Int = x.toInt
    def toLong(x: Short): Long = x.toLong
    def toFloat(x: Short): Float = x.toFloat
    def toDouble(x: Short): Double = x.toDouble
    override def signum(x: Short): Int = math.signum(x.toInt)
    override def sign(x: Short): Short = math.signum(x.toInt).toShort
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
    def parseString(str: String): Option[Byte] = StringParsers.parseByte(str)
    def toInt(x: Byte): Int = x.toInt
    def toLong(x: Byte): Long = x.toLong
    def toFloat(x: Byte): Float = x.toFloat
    def toDouble(x: Byte): Double = x.toDouble
    override def signum(x: Byte): Int = math.signum(x.toInt)
    override def sign(x: Byte): Byte = math.signum(x.toInt).toByte
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
    override def signum(x: Char): Int = math.signum(x.toInt)
    override def sign(x: Char): Char = math.signum(x.toInt).toChar
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
    def parseString(str: String): Option[Long] = StringParsers.parseLong(str)
    def toInt(x: Long): Int = x.toInt
    def toLong(x: Long): Long = x
    def toFloat(x: Long): Float = x.toFloat
    def toDouble(x: Long): Double = x.toDouble
    override def signum(x: Long): Int = math.signum(x).toInt
    override def sign(x: Long): Long = math.signum(x)
  }
  implicit object LongIsIntegral extends LongIsIntegral with Ordering.LongOrdering

  trait FloatIsFractional extends Fractional[Float] {
    def plus(x: Float, y: Float): Float = x + y
    def minus(x: Float, y: Float): Float = x - y
    def times(x: Float, y: Float): Float = x * y
    def negate(x: Float): Float = -x
    def fromInt(x: Int): Float = x.toFloat
    def parseString(str: String): Option[Float] = StringParsers.parseFloat(str)
    def toInt(x: Float): Int = x.toInt
    def toLong(x: Float): Long = x.toLong
    def toFloat(x: Float): Float = x
    def toDouble(x: Float): Double = x.toDouble
    def div(x: Float, y: Float): Float = x / y
    // logic in Numeric base trait mishandles abs(-0.0f)
    override def abs(x: Float): Float = math.abs(x)
    // logic in Numeric base trait mishandles sign(-0.0f) and sign(Float.NaN)
    override def sign(x: Float): Float = math.signum(x)
  }
  implicit object FloatIsFractional extends FloatIsFractional with Ordering.Float.IeeeOrdering

  trait DoubleIsFractional extends Fractional[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def minus(x: Double, y: Double): Double = x - y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def fromInt(x: Int): Double = x.toDouble
    def parseString(str: String): Option[Double] = StringParsers.parseDouble(str)
    def toInt(x: Double): Int = x.toInt
    def toLong(x: Double): Long = x.toLong
    def toFloat(x: Double): Float = x.toFloat
    def toDouble(x: Double): Double = x
    def div(x: Double, y: Double): Double = x / y
    // logic in Numeric base trait mishandles abs(-0.0)
    override def abs(x: Double): Double = math.abs(x)
    // logic in Numeric base trait mishandles sign(-0.0) and sign(Double.NaN)
    override def sign(x: Double): Double = math.signum(x)
  }
  implicit object DoubleIsFractional extends DoubleIsFractional with Ordering.Double.IeeeOrdering

  trait BigDecimalIsConflicted extends Numeric[BigDecimal] {
    // works around pollution of math context by ignoring identity element
    def plus(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._0
      if (x eq _0) y else x + y
    }
    def minus(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._0
      if (x eq _0) -y else x - y
    }
    // works around pollution of math context by ignoring identity element
    def times(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._1
      if (x eq _1) y else x * y
    }
    def negate(x: BigDecimal): BigDecimal = -x
    def fromInt(x: Int): BigDecimal = BigDecimal(x)
    def parseString(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
    def toInt(x: BigDecimal): Int = x.intValue
    def toLong(x: BigDecimal): Long = x.longValue
    def toFloat(x: BigDecimal): Float = x.floatValue
    def toDouble(x: BigDecimal): Double = x.doubleValue
  }
  private object BigDecimalIsConflicted {
    private val _0 = BigDecimal(0)   // cached zero is ordinarily cached for default math context
    private val _1 = BigDecimal(1)   // cached one is ordinarily cached for default math context
  }

  trait BigDecimalIsFractional extends BigDecimalIsConflicted with Fractional[BigDecimal] {
    def div(x: BigDecimal, y: BigDecimal): BigDecimal = x / y
  }
  trait BigDecimalAsIfIntegral extends BigDecimalIsConflicted with Integral[BigDecimal] {
    def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x quot y
    def rem(x: BigDecimal, y: BigDecimal): BigDecimal = x remainder y
  }

  // For BigDecimal we offer an implicit Fractional object, but also one
  // which acts like an Integral type, which is useful in NumericRange.
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional with Ordering.BigDecimalOrdering
  object BigDecimalAsIfIntegral extends BigDecimalAsIfIntegral with Ordering.BigDecimalOrdering
}

trait Numeric[T] extends Ordering[T] {
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

  @deprecated("use `sign` method instead", since = "2.13.0") def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0
  def sign(x: T): T =
    if (lt(x, zero)) negate(one)
    else if (gt(x, zero)) one
    else zero

  class NumericOps(lhs: T) {
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
    def unary_- = negate(lhs)
    def abs: T = Numeric.this.abs(lhs)
    @deprecated("use `sign` method instead", since = "2.13.0") def signum: Int = Numeric.this.signum(lhs)
    def sign: T = Numeric.this.sign(lhs)
    def toInt: Int = Numeric.this.toInt(lhs)
    def toLong: Long = Numeric.this.toLong(lhs)
    def toFloat: Float = Numeric.this.toFloat(lhs)
    def toDouble: Double = Numeric.this.toDouble(lhs)
  }
  implicit def mkNumericOps(lhs: T): NumericOps = new NumericOps(lhs)
}
