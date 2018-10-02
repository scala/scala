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

/*
class From {
  type To[T]
}

class FromSub extends From {
  class To[T] {
    def foo(t: T): T = t
  }
}

object Test {
  implicit def conv[T](x: From): x.To[T] = ???

  val from: FromSub = ???
  conv(from).foo(23)
  //from.foo(23)
}
*/
