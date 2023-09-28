package foo

trait Foo {
  def use(bar: Foo.Bar): Any
}

object Foo {
  class Bar(
      a: Int = 1,
      b: Int = 1,
  ) {
    override def toString(): String = s"Bar($a, $b)"
  }
}
