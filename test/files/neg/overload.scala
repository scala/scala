class C {
  def f(x: int) {}
}

class D extends C {
  def f(x: Any) {}
}

object Test {
  (new D).f(1)
}
