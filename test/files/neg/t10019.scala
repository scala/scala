//> using options -Werror
object Bug {
  sealed case class Foo(e: Option[Int])

  def single(t: Foo): Nothing = t match {
    case Foo(Some(_)) => ???
  }

  def tuple(s: Foo, t: Foo): Nothing = (s, t) match {
    case (Foo(Some(_)), _) => ???
  }
}
