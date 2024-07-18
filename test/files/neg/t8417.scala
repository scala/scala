//> using options -Xfatal-warnings -Xlint:adapted-args
//


trait T {
  def f(x: Any)(y: Any) = "" + x + y
  def g = f("hello", "world")("holy", "moly")
}
