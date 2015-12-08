class C {
  @annotation.implicitAmbiguous("msg A=${A}")
  implicit def f[A](x: Int): String = "f was here"
  implicit def g(x: Int): String = "f was here"
  def test: Unit = {
    implicitly[Int => String]
  }
}

