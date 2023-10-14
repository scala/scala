class Parent {
  def f(x: Int): Parent = ???
  def f: Int = 0

  def g[A](x: Int): Parent = ???
  def g[A]: Int = 0
}

// For the issue to show up, there must be a subclass that overrides
// one of the two methods.
class Sub extends Parent {
  override def f(x: Int): Parent = ???
  override def g[A](x: Int): Parent = ???
}

class C {
  def test(c: Sub): Unit = {
    c.f(1) // already worked
    c.f
    c.f.+(0)
    c.f.toString

    c.g(0) // already worked
    c.g
    c.g[Int]
    c.g.+(0)
    c.g.toString
    c.g[Int].+(0)
    c.g.toString
  }
}
