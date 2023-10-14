class Parent {
  def f(x: Int): Parent = ???
  def f: Int = 0

  def g[A](x: Int): Parent = ???
  def g[A]: Int = 0
}

class Sub extends Parent {
  override def f(x: Int): Parent = ???
  override def g[A](x: Int): Parent = ???
}

class C {
  def bad(c: Sub): Unit = {
    c.f: String // error
    c.g: String // error
    c.f.bad // error
    c.g.bad // error

    c.f("") // error
    c.g("") // error
    c.g[Int]("") // error
    c.g[Int]: (String => String) // error
    c.g[Int]: (Int => Parent) // ok
  }
}
