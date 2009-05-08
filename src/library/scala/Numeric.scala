package scala

object Numeric {
  implicit object IntIsIntegral extends Integral[Int] {
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
  implicit object LongIsIntegral extends Integral[Long] {
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
  implicit object FloatIsFractional extends Fractional[Float] {
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
  implicit object DoubleIsFractional extends Fractional[Double] {
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
}
