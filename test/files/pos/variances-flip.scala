trait Foo[-A, +B, -C, +D] {
  private[this] def b: B = ???
  private[this] def d: D = ???

  def f(p1: B => A, p2: D => C) = g(p1(b), p2(d))
  def g(x: A, y: C) = ((b, d))
}
