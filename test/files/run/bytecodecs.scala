import scala.reflect.internal.pickling.ByteCodecs._

object Test {

  def test8to7(xs: Array[Byte]) {
    val ys = encode8to7(xs)
    decode7to8(ys, ys.length)
    assert(ys.take(xs.length).deep == xs.deep,
           "test8to7("+xs.deep+") failed, result = "+ys.take(xs.length).deep)
  }

  def testAll(xs: Array[Byte]) {
    val ys = encode(xs)
    decode(ys)
    assert(ys.take(xs.length).deep == xs.deep,
           "testAll("+xs.deep+") failed, result = "+ys.take(xs.length).deep)
  }

  def test(inputs: Array[Byte]*) {
    for (input <- inputs) {
      test8to7(input)
      testAll(input)
    }
  }

  def main(args: Array[String]) {
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
