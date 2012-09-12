object Test {
  case class Foo(x: Int, y: Int)
  case class Bar(x: AnyRef)

  def f(x: Any) = x match { case Bar(Foo(5)) => }
}
