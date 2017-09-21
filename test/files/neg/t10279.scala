object Test {

  def foo(i: Int)(implicit s: String): String = ???

  def test(implicit s: String) {
    // foo(1) _
  }

  val bar = foo(1) _

  def fooSimple(implicit x: Int): Int = x
  val barSimple = fooSimple _
}
