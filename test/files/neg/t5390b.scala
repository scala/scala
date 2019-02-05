class A {
  case class B(s: String)
}

object X {
  def foo: Unit = {
    val b = a.B("")
    val a = new A
  }
}
