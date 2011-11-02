// a quickly assembled test of equality.  Needs work.
object Test
{
  import scala.runtime.ScalaRunTime.hash
  
  def makeFromInt(x: Int) = List(
    x.toByte, x.toShort, x.toInt, x.toLong, x.toFloat, x.toDouble, BigInt(x), BigDecimal(x)
  ) ::: (
    if (x < 0) Nil else List(x.toChar)
  )
  def makeFromDouble(x: Double) = List(
    x.toShort, x.toInt, x.toLong, x.toFloat, x.toDouble, BigInt(x.toInt), BigDecimal(x)
  )
  
  def main(args: Array[String]): Unit = {
    var xs = makeFromInt(5)
    for (x <- xs ; y <- xs) {
      assert(x == y, x + " == " + y)
      assert(hash(x) == hash(y), "hash(%s) == hash(%s)".format(x, y))
    }
    
    xs = makeFromInt(-5)
    for (x <- xs ; y <- xs) {
      assert(x == y, x + " == " + y)
      assert(hash(x) == hash(y), "hash(%s) == hash(%s)".format(x, y))
    }
    
    xs = makeFromDouble(500.0)
    for (x <- xs ; y <- xs) {
      assert(x == y, x + " == " + y)
      assert(hash(x) == hash(y), "hash(%s) == hash(%s)".format(x, y))
    }
    
    // negatives
    val bigLong = new java.util.concurrent.atomic.AtomicLong(Long.MaxValue)
    assert(-1 != bigLong && bigLong != -1)  // bigLong.intValue() == -1
    assert(BigDecimal(1.1) != 1L)
    assert(1L != BigDecimal(1.1))
  }
}
