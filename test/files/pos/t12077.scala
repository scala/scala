object Main {
  type Foo[+A] <: A

  final class Bar {
    def bar = ???
  }

  class Ops[A](private val self: Foo[A]) {
    def baz: A = self match {
      case x: Bar => x.bar
    }
  }
}
