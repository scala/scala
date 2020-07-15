package tastytest

class Overloader {
  def foo: Int = 13
  def foo(a: String): String = a + a
  def foo(a: String, b: Boolean): String = a + b
}
