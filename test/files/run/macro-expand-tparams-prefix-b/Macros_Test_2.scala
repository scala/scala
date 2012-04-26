object Test extends App {
  class C[T] {
    def foo[U](x: U) = macro Impls.foo[T, U]
  }

  object D extends C[Boolean]

  D.foo(42)
  D.foo("42")
}