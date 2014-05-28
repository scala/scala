class Bar {
  def bar(b: Int = 2) {}; def baz[X](b: Int = 2) {}
}

class Foo {
  def foo() {
    new Bar/*#*/().bar/*#*/()
    new Bar/*#*/().baz/*#*/[Any]()
    new Bar/*#*/().baz/*#*/()
  }
}
