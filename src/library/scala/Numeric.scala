package scala

object Numeric {
   trait BigIntIsIntegral extends Integral[BigInt] {
    def plus(x: BigInt, y: BigInt): BigInt = x + y
    def minus(x: BigInt, y: BigInt): BigInt = x - y
    def times(x: BigInt, y: BigInt): BigInt = x * y
    def quot(x: BigInt, y: BigInt): BigInt = x / y
    def rem(x: BigInt, y: BigInt): BigInt = x % y
    def negate(x: BigInt): BigInt = -x
    def abs(x: BigInt): BigInt = if (x < 0) -x else x
    def signum(x: BigInt): BigInt = if (x < 0) -1 else if (x > 0) 1 else 0
    def fromInt(x: Int): BigInt = BigInt(x)
    def toInt(x: BigInt): Int = x.intValue
    def toLong(x: BigInt): Long = x.longValue
    def toFloat(x: BigInt): Float = x.longValue.toFloat
    def toDouble(x: BigInt): Double = x.longValue.toDouble
  }
  implicit object BigIntIsIntegral extends BigIntIsIntegral

  trait IntIsIntegral extends Integral[Int] {
    def plus(x: Int, y: Int): Int = x + y
    def minus(x: Int, y: Int): Int = x - y
    def times(x: Int, y: Int): Int = x * y
    def quot(x: Int, y: Int): Int = x / y
    def rem(x: Int, y: Int): Int = x % y
    def negate(x: Int): Int = -x
    def abs(x: Int): Int = if (x < 0) -x else x
    def signum(x: Int): Int = if (x < 0) -1 else if (x > 0) 1 else 0
    def fromInt(x: Int): Int = x
    def toInt(x: Int): Int = x
    def toLong(x: Int): Long = x
    def toFloat(x: Int): Float = x
    def toDouble(x: Int): Double = x
  }
  implicit object IntIsIntegral extends IntIsIntegral

  trait LongIsIntegral extends Integral[Long] {
    def plus(x: Long, y: Long): Long = x + y
    def minus(x: Long, y: Long): Long = x - y
    def times(x: Long, y: Long): Long = x * y
    def quot(x: Long, y: Long): Long = x / y
    def rem(x: Long, y: Long): Long = x % y
    def negate(x: Long): Long = -x
    def abs(x: Long): Long = if (x < 0) -x else x
    def signum(x: Long): Long = if (x < 0) -1 else if (x > 0) 1 else 0
    def fromInt(x: Int): Long = x
    def toInt(x: Long): Int = x.toInt
    def toLong(x: Long): Long = x
    def toFloat(x: Long): Float = x
    def toDouble(x: Long): Double = x
  }
  implicit object LongIsIntegral extends LongIsIntegral

  trait FloatIsFractional extends Fractional[Float] {
    def plus(x: Float, y: Float): Float = x + y
    def minus(x: Float, y: Float): Float = x - y
    def times(x: Float, y: Float): Float = x * y
    def div(x: Float, y: Float): Float = x / y
    def negate(x: Float): Float = -x
    def abs(x: Float): Float = if (x < 0) -x else x
    def signum(x: Float): Float = if (x < 0) -1 else if (x > 0) 1 else 0
    def fromInt(x: Int): Float = x
    def toInt(x: Float): Int = x.toInt
    def toLong(x: Float): Long = x.toLong
    def toFloat(x: Float): Float = x
    def toDouble(x: Float): Double = x
  }
  implicit object FloatIsFractional extends FloatIsFractional

  trait DoubleIsFractional extends Fractional[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def minus(x: Double, y: Double): Double = x - y
    def times(x: Double, y: Double): Double = x * y
    def div(x: Double, y: Double): Double = x / y
    def negate(x: Double): Double = -x
    def abs(x: Double): Double = if (x < 0) -x else x
    def signum(x: Double): Double = if (x < 0) -1 else if (x > 0) 1 else 0
    def fromInt(x: Int): Double = x
    def toInt(x: Double): Int = x.toInt
    def toLong(x: Double): Long = x.toLong
    def toFloat(x: Double): Float = x.toFloat
    def toDouble(x: Double): Double = x
  }
  implicit object DoubleIsFractional extends DoubleIsFractional

}

trait Numeric[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def abs(x: T): T
  def signum(x: T): T
  def fromInt(x: Int): T
  def toInt(x: T): Int
  def toLong(x: T): Long
  def toFloat(x: T): Float
  def toDouble(x: T): Double
  def zero = fromInt(0)
  def one = fromInt(1)

  class Ops(lhs: T) {
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
    def unary_-() = negate(lhs)
    def abs(): T = Numeric.this.abs(lhs)
    def signum(): T = Numeric.this.signum(lhs)
    def toInt(): Int = Numeric.this.toInt(lhs)
    def toLong(): Long = Numeric.this.toLong(lhs)
    def toFloat(): Float = Numeric.this.toFloat(lhs)
    def toDouble(): Double = Numeric.this.toDouble(lhs)
  }
  implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)
}
