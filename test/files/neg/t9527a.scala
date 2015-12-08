class C {
  implicit def f(x: Int): String = "f was here"
  implicit def g(x: Int): String = "f was here"
  def test: Unit = {
    implicitly[Int => String]
  }
}

