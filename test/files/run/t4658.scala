import scala.collection.immutable.NumericRange
//#4658
object Test {

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
    // We drop the first two tests for all ranges which don't have a decent sum implementation,
    // because it is just too slow.
    println("Ranges:")
    ranges.foreach{range => println(range.sum)}
    println("IntRanges:")
    println("Disabled #1")
    println("Disabled #2")
    numericIntRanges.drop(2).foreach{range => println(range.sum)}
    println("LongRanges:")
    println("Disabled #1")
    println("Disabled #2")
    numericLongRanges.drop(2).foreach{range => println(range.sum)}
    println("BigIntRanges:")
    println("Disabled #1")
    println("Disabled #2")
    numericBigIntRanges.drop(2).foreach{range => println(range.sum)}
  }

}