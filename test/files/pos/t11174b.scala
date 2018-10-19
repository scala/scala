class From {
  class To[T] {
    def foo(t: T): T = t
  }
}

object Test {
  implicit def conv[T](x: From): x.To[T] = ???

  val from: From = ???
  from.foo(23)
}
