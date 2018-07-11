import scala.tools.partest.Util.ArrayDeep
import scala.reflect.internal.pickling.ByteCodecs._

object Test {

  def test8to7(xs: Array[Byte]): Unit = {
    val ys = encode8to7(xs)
    decode7to8(ys, ys.length)
    assert(ys.take(xs.length).prettyArray == xs.prettyArray,
           "test8to7("+xs.prettyArray+") failed, result = "+ys.take(xs.length).prettyArray)
  }

  def testAll(xs: Array[Byte]): Unit = {
    val ys = encode(xs)
    decode(ys)
    assert(ys.take(xs.length).prettyArray == xs.prettyArray,
           "testAll("+xs.prettyArray+") failed, result = "+ys.take(xs.length).prettyArray)
  }

  def test(inputs: Array[Byte]*): Unit = {
    for (input <- inputs) {
      test8to7(input)
      testAll(input)
    }
  }

  def main(args: Array[String]): Unit = {
    test(
      Array(1, 2, 3),
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(1, -2, 0, -3, -5, -6, -7),
      Array(1, 3, -1, -128, 0, 0, -128, 1, 2, 3))
    val rand = new scala.util.Random()
    for (i <- 1 until 5000) {
      var xs = new Array[Byte](i)
      rand.nextBytes(xs)
      test(xs)
    }
  }
}
