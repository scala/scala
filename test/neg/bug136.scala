class Foo;
class Bar: Foo  {
  val bar: Bar = null;
  val foo1: Foo = this;
  val foo2: Foo = bar;
}
object bar: Foo  {
  val foo1: Foo = this;
  val foo2: Foo = bar;
}
