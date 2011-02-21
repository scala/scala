object This extends App {
  trait A {
    def foo(): Unit
  }
  class C { self: A =>
    def bar() = this.foo()
  }
  class D extends C with A {
    def foo() = ()
  }
  val c: C = new D
}
