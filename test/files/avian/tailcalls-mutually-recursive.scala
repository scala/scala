object Test extends App {
  def f(a: Long, b: Long = 0): Unit =
    if (a <= 0) println(s"f finished: $b")
    else g(a-1, b+1)

  def g(a: Long, b: Long = 0): Unit =
    if (a <= 0) println(s"g finished: $b")
    else f(a-1, b+1)

  f(10000, 0)
  g(10000, 0)
  f(9999, 0)
  g(9999, 0)
}
