class A {
  case class B(s: String)
}

object X {
  def foo {
    val b = a.B("")
    val a = new A
  }
}