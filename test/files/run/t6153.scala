import math.{BigDecimal,BigInt}
import scala.util.Random
import java.math.MathContext

object Test {
  val dmc = new MathContext(5000) //default math context, lots of precision
  val r = new Random(0)
  val numRands = 100

  def checkHashCode(num: String) = {
    val bi = BigInt(num)
    val bd = BigDecimal(num, dmc)

    assert(bi != bd || bi.## == bd.##, "BigInt.hashCode != BigDecimal.hashCode for value %s" format num)
  }

  def checkHashCode(num: Long) = {
    val i: Int = num toInt
    val l: Long = num
    val bi = BigInt(l)
    val bd = BigDecimal(l, dmc)
    val str: String = num.toString 

    assert(l != i || i.## == l.##, "Int.hashCode != Long.hashCode for value %s" format str)
    assert(l != bi || l.## == bi.##, "Long.hashCode != BigInt.hashCode for value %s" format str)
    assert(bi != bd || bi.## == bd.##, "BigInt.hashCode != BigDecimal.hashCode for value %s" format str)
  }

  def checkHashCode(num: Double) = {
    val bd = BigDecimal(num, dmc)
    val bi = bd toBigInt
    val str: String = num toString

    if (num.isValidInt && bi.isValidDouble) {
      assert(num != bi || num.## == bi.##, "Double.hashCode != BigInt.hashCode for value %s" format str)
      assert(bi != bd || bi.## == bd.##, "BigInt.hashCode != BigDecimal.hashCode for value %s" format str)
    }

    if (bd.isValidDouble) {
      assert(num != bd || num.## == bd.##, "Double.hashCode != BigDecimal.hashCode for value %s" format str)
    }
  }
  
  def allSameHash(things: Any*) = { val size = (things map ((i) => i.##)).toSet.size; size == 0 || size == 1 }

  def main(args: Array[String]): Unit = {

    //Ints
    checkHashCode(0)
    checkHashCode(1)
    checkHashCode(-342342)
    checkHashCode(Int.MaxValue / 2)
    checkHashCode(Int.MaxValue)
    checkHashCode(Int.MinValue)
    for (i <- 1 to numRands) { checkHashCode(r nextInt) }

    //Longs
    checkHashCode(0L)
    checkHashCode(1L)
    checkHashCode(-342342L)
    checkHashCode(Int.MaxValue / 2L)
    checkHashCode(Int.MaxValue toLong)
    checkHashCode(Int.MinValue toLong)
    checkHashCode(Long.MaxValue)
    checkHashCode(Long.MinValue)
    for (i <- 1 to numRands) { checkHashCode(r nextLong) }

    //Doubles
    checkHashCode(0D)
    checkHashCode(1D)
    checkHashCode(-342342D)
    checkHashCode(math.floor(Int.MaxValue / 2D))
    checkHashCode(Int.MaxValue.toDouble)
    checkHashCode(Int.MinValue.toDouble)
    checkHashCode(Long.MaxValue.toDouble)
    checkHashCode(Long.MinValue.toDouble)

    //Need to lose some precision to make valid double conversions
    checkHashCode((Long.MaxValue >> 8).toDouble)
    checkHashCode((Long.MinValue >> 8).toDouble)
    checkHashCode(Double.MinPositiveValue)
    checkHashCode(Double.MaxValue)
    checkHashCode(Double.MinValue)
    for (i <- 1 to numRands) { checkHashCode(r nextDouble) }

    //Bigs
    checkHashCode("0")
    checkHashCode("239847")
    checkHashCode("-729831027830346579")
    checkHashCode("239847938465823964587234987")
    checkHashCode("29384709835692847502983470236512906984")

    //Same hashCode for same value for different precision
    val a = BigDecimal(2, new MathContext(20))
    val b = BigDecimal(2, new MathContext(50))
    val c = BigDecimal(2.000)
    assert(allSameHash(a, b, c), "BigDecimal generates different hashCodes for different precisions!")

    val preciseDecimalStr = "73029895032016298735012347895.40198263978501236495732169"
    val precision = preciseDecimalStr.length - 1
    val d = BigDecimal(preciseDecimalStr, new MathContext(precision + 1))
    val e = BigDecimal(preciseDecimalStr, new MathContext(2 * precision))
    val f = BigDecimal(preciseDecimalStr + "0000", new MathContext(2 * precision))
    assert(allSameHash(d, e, f), "BigDecimal generates different hashCodes for different precisions!")

    val endsWith0 = "1129384720000000000000000000000000000"
    val g = BigDecimal(endsWith0, dmc)
    val h = BigDecimal(endsWith0 + ".0", dmc)
    val i = BigDecimal(endsWith0 + ".00000", dmc)
    assert(allSameHash(g, h, i), "BigDecimal generates different hashCodes for different precisions!")

    //Check neighbors don't have same hashCode
    val doubleEpsilon: BigDecimal = BigDecimal(Double.MinPositiveValue)     //4.9E-324
    val subLong: BigDecimal = BigDecimal(Long.MaxValue / 2)
    val hugeInt: BigInt = BigInt(2) pow 403
    val hugeDecimal: BigDecimal = BigDecimal(hugeInt, new MathContext(5000))

    val smallNumbers:    Set[Int] = (1 to 100) map (i => (doubleEpsilon * i / 1000).##) toSet
    val largeNumbers:    Set[Int] = (1 to 100) map (i => (subLong + i + 0.1).##) toSet
    val flippedBitsInt:  Set[Int] = (1 to 100) map (i => (hugeInt * BigInt(2) pow i).##) toSet
    val flippedBitsDec:  Set[Int] = (1 to 100) map (i => (hugeDecimal * (BigDecimal(2) pow i) + 0.1).##) toSet

    println("smallNumbers.size = %s" format smallNumbers.size)
    println("largeNumbers.size = %s" format smallNumbers.size)
    println("flippedBitsInt.size = %s" format flippedBitsInt.size)
    println("flippedBitsDec.size = %s" format flippedBitsDec.size)
  }
}
