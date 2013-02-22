trait T {
  final case class A()
}

final class U extends T {
  // this match should also not be deemed impossible
  def foo(a: U#A) = a match {
    case _: A => true; case _ => false
  }

  // this match should also not be deemed impossible
  def bar(a: T#A) = a match {
    case _: A => true; case _ => false
  }
}
