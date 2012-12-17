class A {
  object B { def apply(s: String) = 0}
}

object X {
  def foo {
    val b = a.B("")
    val a = new A
  }
}