// This only tests direct access to the methods in ScalaRunTime,
// not the whole scheme.
object Test
{
  import java.{ lang => jl }
  import scala.runtime.ScalaRunTime.{ hash }
  
  def allSame[T](xs: List[T]) = assert(xs.distinct.size == 1, "failed: " + xs)
  
  def mkNumbers(x: Int): List[Number] =
    List(x.toByte, x.toShort, x, x.toLong, x.toFloat, x.toDouble)
  
  def testLDF(x: Long) = allSame(List[Number](x, x.toDouble, x.toFloat) map hash)
   
  def main(args: Array[String]): Unit = {
    List(Byte.MinValue, -1, 0, 1, Byte.MaxValue) foreach { n => 
      val hashes = mkNumbers(n) map hash
      allSame(hashes)
      if (n >= 0) {
        val charCode = hash(n.toChar: Character)
        assert(charCode == hashes.head)
      }
    }
    
    testLDF(Short.MaxValue.toLong)
    testLDF(Short.MinValue.toLong)
  }
}
