class SameBytecode {
  def foo(a: Int, b: String) = 0
  def foo(a: Int, b: Any) = 0

  def a = foo(0, "")
  def b = foo(a = 0, "")
  def c = foo(0, b = "")
  def d = foo(a = 0, b = "")
}