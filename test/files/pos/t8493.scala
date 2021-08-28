object Test {
  trait Foo {
    def foo: this.type
  }

  case class Impl() extends Foo {
    def foo = ???
    def bar: Unit = ()
  }

  object Foo {
    def foo(f: Foo): f.type = f.foo
  }

  def work(f: Impl): Unit =
    Foo.foo(f).bar

  def bug(f: Int => Impl): Unit =
    Foo.foo(f(1)).bar

  def workaround(f: Int => Impl): Unit = {
    val tmp = f(1)
    Foo.foo(tmp).bar
  }
}
