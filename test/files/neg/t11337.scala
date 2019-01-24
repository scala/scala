trait Foo[T]
object Foo {
  val foo: Foo[Any] { type Bar = Any } = new Foo { def baz(): Any = () }
}
