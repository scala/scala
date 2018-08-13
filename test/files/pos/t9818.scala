trait A {
  def g(x: Int = 0, y: Int = 1) = x + y

  def x: Int = ???

  def ref: A
}

trait B {
  def f(a: Int, b: Int = 0) = a + b

  def foo(in: A): Unit = {
    import in._

    ref.g(x = f(0))
  }
}
