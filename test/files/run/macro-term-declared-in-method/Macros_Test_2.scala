object Test extends App {
  def bar() = {
    def foo: Unit = macro Impls.foo
    foo
  }

  bar()
}