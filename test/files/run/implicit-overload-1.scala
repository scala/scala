class Bar
object Bar {
  implicit val bar: Bar = new Bar
}

class Baz extends Bar

object Foo {
  def nullary: String = "foo0"
  def nullary(implicit bar: Bar): Int = 23
  def nullary(bar: Bar): Boolean = true

  def unary(i: Int): String = "foo2"
  def unary(i: Int)(implicit bar: Bar): Int = i

  def unary1(bar: Bar): String = "foo4"
  def unary1(baz: Baz): Int = 11
  def unary1(bar: Bar)(implicit bar2: Bar): Boolean = true
}

object Test extends App {
  val foo0: String = Foo.nullary
  assert(foo0 == "foo0")

  val foo1: Int = Foo.nullary
  assert(foo1 == 23)

  val foo2: String = Foo.unary(13)
  assert(foo2 == "foo2")

  val foo3: Int = Foo.unary(17)
  assert(foo3 == 17)

  val foo4: String = Foo.unary1(new Bar)
  assert(foo4 == "foo4")

  val foo5: String = Foo.unary1(new Baz)
  assert(foo5 == "foo4")

  val foo6: Boolean = Foo.unary1(new Baz)
  assert(foo6)

  val foo7: Int = Foo.nullary(new Bar)
  assert(foo7 == 23)

  val foo8: Boolean = Foo.nullary(new Bar)
  assert(foo8)
}
