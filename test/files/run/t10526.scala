object Test extends App {
  class Foo
  class Bar extends Foo

  def overload(implicit foo: Foo): Int = 0
  def overload(implicit bar: Bar): Int = 1

  assert(overload(new Bar) == 1)
}
