
import scala.math.BigDecimal

object Test extends App {
  println(1 to 10)
  println(1 to 10 by 1)
  println(1 to 10 by 2)
  println(1 to 10 by 3)
  println(1 until 10 by 2)
  println(100 to 100)
  println(100 until 100)

  println(1L to 10L)
  println(1L to 10L by 2)

  // want to know if this is BigDecimal or Double stepping by BigDecimal
  println(0.1 until 1.0 by 0.1)
  println(Range.BigDecimal(BigDecimal("0.1"), BigDecimal("1.0"), BigDecimal("0.1")))
  println(Range.Double(0.1, 1.0, 0.1))

  import concurrent.duration.{SECONDS => Seconds, _}, collection.immutable.NumericRange
  implicit val `duration is integerish`: math.Integral[FiniteDuration] = new math.Integral[FiniteDuration] {
    def quot(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???
    def rem(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???

    // Members declared in scala.math.Numeric
    def fromInt(x: Int): scala.concurrent.duration.FiniteDuration = Duration(x, Seconds)
    def minus(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???
    def negate(x: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???
    def plus(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???
    def times(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): scala.concurrent.duration.FiniteDuration = ???
    def toDouble(x: scala.concurrent.duration.FiniteDuration): Double = ???
    def toFloat(x: scala.concurrent.duration.FiniteDuration): Float = ???
    def toInt(x: scala.concurrent.duration.FiniteDuration): Int = toLong(x).toInt
    def toLong(x: scala.concurrent.duration.FiniteDuration): Long = x.length

    // Members declared in scala.math.Ordering
    def compare(x: scala.concurrent.duration.FiniteDuration,y: scala.concurrent.duration.FiniteDuration): Int =
      x.compare(y)
  }
  println(NumericRange(Duration.Zero, Duration(10, Seconds), Duration(1, Seconds)))
  println(NumericRange(Duration.Zero, Duration.Zero, Duration(1, Seconds)))
}
