object Test extends App {
  def bar() = {
    def foo = macro Impls.foo
    foo
  }

  bar()
}