object test {
  import A._
  class A(b: B = new A.B())
  object A {
    class B
    new A()
  }
}

