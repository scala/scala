object Test {
  class Foo()(implicit ev: Nothing)
  new Foo(): String
}
