class Y(val _2: Int, val _1: String)

object X { def unapply(u: Unit): Option[Y] = Some(new Y(42, "!")) }

object Test {
  def test1 = {
    val X(y) = ()
    val yy: Y = y
    assert(yy._1 == "!")
    assert(yy._2 == 42)
  }
  def main(args: Array[String]): Unit = {
    test1
  }
}
