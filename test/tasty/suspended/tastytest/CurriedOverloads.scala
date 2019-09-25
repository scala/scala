package tastytest

class CurriedOverloads {
  def foo: String = "foo"
  def foo(msg: String): String = msg
  def foo(i: Int)(msg: String): String = msg + i
}