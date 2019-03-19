case class Foo(x: Int)

trait A[X]  {
  def concat[Dummy](suffix: Int): Dummy = ???
}

class Bar extends A[Foo] {
  def concat(suffix: Int): Foo = Foo(0)
}
