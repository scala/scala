object Test {
  sealed trait Foo
  object Bar extends Foo

  object Other
  val OtherAlias = Other

  def f(x: Foo) = x match {
    case Other => true // warns
    case _     => false
  }

  def g(x: Foo) = x match {
    case OtherAlias => true // no warning.
    case _          => false
  }
}
