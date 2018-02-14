class C {
  def f(x: Int): Unit = {}
}

class D extends C {
  def f(x: Any): Unit = {}
}

object Test {
  (new D).f(1)
}
