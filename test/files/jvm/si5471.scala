
object Test {

  def main(args: Array[String]) {
    import scala.math.Numeric
    import scala.math.Numeric.Implicits._

    val b = BigInt(Long.MaxValue) + 1

    def dbl[N :Numeric](n: N) = n.toDouble
    def flt[N :Numeric](n: N) = n.toFloat

    println(dbl(b) == b.toDouble)
    println(flt(b) == b.toFloat)
  }

}
