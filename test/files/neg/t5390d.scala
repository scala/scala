class A {
  case class B(s: String)
}

object X {
  def foo {
    val b = a.B.toString
    val a = new A
  }
}