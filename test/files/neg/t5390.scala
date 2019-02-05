class A {
  object B { def apply(s: String) = 0}
}

object X {
  def foo: Unit = {
    val b = a.B("")
    val a = new A
  }
}
