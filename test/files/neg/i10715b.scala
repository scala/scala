class Parent {
  def f(x: Int): Unit = ()
  def f: Int = 0
}

class Sub extends Parent {
  override def f(x: Int): Unit = ()
  def f(x: Int)(implicit s: String): Unit = ()
}

class C {
  def bad(c: Sub): Unit = c.f(1) // error: ambiguous overload
}
