object Test {
  class Foo(val bar: Bar)
  class Bar(baz0: => Baz) {
    lazy val baz = baz0
  }
  class Baz(val foo: Foo)

  implicit def foo(implicit bar: Bar): Foo = new Foo(bar)
  implicit def bar(implicit baz: => Baz): Bar = new Bar(baz)
  implicit def baz(implicit foo: Foo): Baz = new Baz(foo)

  def main(args: Array[String]): Unit = {
    val foo = implicitly[Foo]
    assert(foo.bar.baz.foo eq foo)
  }
}
