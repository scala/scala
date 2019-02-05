class A {
  case class B[A](s: String)
}

object X {
  def foo: Unit = {
    val a = new A
    val b = new a.B[c.type]("") // not a forward reference
    val c = ""
  }
}
