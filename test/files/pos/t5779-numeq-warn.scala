
object Test {
  def main(args: Array[String]) {
    val d: Double = (BigInt(1) << 64).toDouble
    val f: Float = d.toFloat
    val n: java.lang.Number = d.toFloat
    assert (d == f)   // ok
    assert (d == n)   // was: comparing values of types Double and Number using `==' will always yield false
    assert (n == d)   // was: Number and Double are unrelated: they will most likely never compare equal
    assert (f == n)
    assert (n == f)
  }
}
