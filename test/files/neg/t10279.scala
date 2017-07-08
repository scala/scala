object Test {

  def foo(i: Int)(implicit s: String): String = ???

  def test(implicit s: String) {
    // foo(1) _
  }

  foo(1) _
}
