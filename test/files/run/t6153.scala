import math.{BigDecimal,BigInt}

object Test {
  val doubleEpsilon: BigDecimal = BigDecimal(Double.MinPositiveValue)     //4.9E-324
  val subLong: BigDecimal = BigDecimal(Long.MaxValue / 2)
  val smallNumbers: Set[Int] = (1 to 100) map (i => (doubleEpsilon * i / 1000).##) toSet
  val largeNumbers: Set[Int] = (1 to 100) map (i => (subLong + i + 0.1).##) toSet

  def main(args: Array[String]): Unit = {
    val i: BigInt = 1000000
    val d: BigDecimal = 1000000
    val i4 = i pow 4
    val d4 = d pow 4
    assert(i4 == d4, s"$i4 != $d4")
    assert(i4.hashCode == d4.hashCode, s"${i4.hashCode} == ${d4.hashCode}")
    println(s"smallNumbers.size = ${smallNumbers.size}")
    println(s"largeNumbers.size = ${largeNumbers.size}")
  }
}
