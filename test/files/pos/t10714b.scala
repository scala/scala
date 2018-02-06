object Test {
  class Bar {
    class Foo {
      type Out
    }
    object Foo {
      implicit def foo: Foo { type Out = Bar } = ???
    }

    def foo(implicit foo: Foo): foo.Out = ???
  }

  (new Bar).foo.foo
}
