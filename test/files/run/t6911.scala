trait K {
  case class CC(name: String)
  case class DD[+A1, A2](x1: A1, x2: A2)
}

object Test {
  object Foo extends K
  object Bar extends K

  val b1 = Foo.CC("b")
  val b2 = Bar.CC("b")
  val b3 = Foo.CC("b")

  val c1 = Foo.DD("a", 5)
  val c2 = Bar.DD("a", 5)
  val c3 = Foo.DD("a", 5)

  def main(args: Array[String]): Unit = {
    assert(b1 != b2, ((b1, b2))) // false under 2.9, true under 2.10-RC5
    assert(b1 == b3, ((b1, b3)))
    assert(c1 != c2, ((c1, c2)))
    assert(c1 == c3, ((c1, c3)))
  }
}
