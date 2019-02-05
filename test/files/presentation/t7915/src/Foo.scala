class Bar {
  def bar(b: Int = 2): Unit = {}; def baz[X](b: Int = 2): Unit = {}
}

class Foo {
  def foo(): Unit = {
    new Bar/*#*/().bar/*#*/()
    new Bar/*#*/().baz/*#*/[Any]()
    new Bar/*#*/().baz/*#*/()
  }
}
