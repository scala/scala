class Test(private[this] val x: String) {
  lazy val y = x.reverse
}
object Test {
  def main(args: Array[String]): Unit = {
    val t = new Test("foo")
    assert(t.y == "oof", t.y)
  }
}

