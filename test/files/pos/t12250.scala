//> using options -Werror
final case class Foo(value: String)

object Foo {
  def unapply(str: String): Option[Foo] = Some(Foo(str))

  def extract(id: Foo): String =
    id match {
      case Foo(a) => a
    }
}
