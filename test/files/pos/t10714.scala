object Test {
  class Foo {
    type Out
  }
  object Foo {
    implicit def foo: Foo { type Out = Bar } = ???
  }

  class Bar {
    type Baz = Foo
    def foo(implicit foo: Baz): foo.Out = ???
  }

  (new Bar).foo.foo
}
