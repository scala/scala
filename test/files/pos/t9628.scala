case class Foo(bar: String, foo: String)
case class Bar(bar: String)

object FooBar {
  def crash(): Unit = {
    val foo = Foo("foo", "bar").copy(foo = "foo")
    val bar = Bar(foo.bar)
  }
}
