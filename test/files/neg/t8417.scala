

trait T {
  def f(x: Any)(y: Any) = "" + x + y
  def g = f("hello", "world")("holy", "moly")
}
