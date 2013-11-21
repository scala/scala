class Bar {
  def bar(b: Int = 2) {}
}

class Foo {
  def foo() {
    new Bar/*#*/().bar/*#*/()
  }
}
