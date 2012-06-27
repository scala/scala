object Test {
  def goo[T](x: Int => T): T = x(1)
  implicit def f(x: Int): String = ""
  def foo(x: Int): Int = x + 1
  val x: String = goo(foo _)
  def foo(x: String => String) = 1
}
