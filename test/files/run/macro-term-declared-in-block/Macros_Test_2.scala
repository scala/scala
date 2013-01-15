object Test extends App {
  {
    def foo = macro Impls.foo
    foo
  }
}