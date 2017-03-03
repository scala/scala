class Test(private val x: String) {
  lazy val y = x.reverse
}
object Test {
  def getX(t: Test) = t.x
  def main(args: Array[String]): Unit = {
    val t = new Test("foo")
    assert(t.y == "oof", t.y)
    assert(t.x == "foo", t.x)
  }
}

