import scala.collection.immutable.NumericRange
//#4658
object Test {

  // Only works for Int values!  Need to rethink explicit otherwise.
  case class R(start: Int, end: Int, step: Int = 1, inclusive: Boolean = true)

  val rangeData = Array(
    R(1, Int.MaxValue), R(-Int.MaxValue, -1), R(0, 0), R(0,0, inclusive = false), R(1,10),
    R(1,10,2), R(1,10,11), R(-10, -5), R(-10, 0), R(-10, 10), R(-10, -5, 2), R(-10, 0, 2), R(-10, 10, 2),
    R(-10, -5, inclusive = false), R(-10, 0, inclusive = false), R(-10, 10, inclusive = false),
    R(-10, -5, 2, inclusive = false), R(-10, 0, 2, inclusive = false), R(-10, 10, 2, inclusive = false)
  )

  def ranges = rangeData.map(r => if (r.inclusive) r.start to r.end by r.step else r.start until r.end by r.step)

  def numericIntRanges = rangeData.map(r => if (r.inclusive) NumericRange.inclusive(r.start, r.end, r.step) else NumericRange(r.start, r.end, r.step))

  def numericLongRanges = rangeData.map(r => if (r.inclusive) NumericRange.inclusive(r.start.toLong, r.end, r.step) else NumericRange(r.start.toLong, r.end, r.step))

  def numericBigIntRanges = rangeData.map(r => if (r.inclusive) NumericRange.inclusive(BigInt(r.start), BigInt(r.end), BigInt(r.step)) else NumericRange(BigInt(r.start), BigInt(r.end), BigInt(r.step)))

  def main(args: Array[String]) {
    println("Ranges:")
    ranges.foreach{range => println(range.sum)}
    println("IntRanges:")
    numericIntRanges.foreach{range => println(range.sum)}
    println("LongRanges:")
    numericLongRanges.foreach{range => println(range.sum)}
    println("BigIntRanges:")
    numericBigIntRanges.foreach{range => println(range.sum)}
    println("BigInt agrees with Long: " + 
      (numericLongRanges zip numericBigIntRanges).forall{ case (lr, bir) => lr.sum == bir.sum }
    )
    println("Long agrees with Int when rounded: " + 
      (numericLongRanges zip numericIntRanges).forall{ case (lr, ir) => lr.sum.toInt == ir.sum }
    )
    println("Numeric Int agrees with Range: " +
      (numericIntRanges zip ranges).forall{ case (ir, r) => ir.sum == r.sum }
    )
  }
}