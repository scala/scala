object Test extends App {
  class C[T] {
    def foo[U](x: U) = macro Impls.foo[U]
  }

  new C[Int]().foo(42)
  new C[Boolean]().foo(42)
  new C[Int]().foo("42")
  new C[String]().foo(true)
}