class Bar(val x: Int) extends AnyVal {
  def f: String = f(0)
  private def f(x: Int): String = ""
}

class Baz(val x: Int) extends AnyVal {
  def f: String = "123"
  private def f(x: Int): String = ""
}
object Baz {
  def x(b: Baz) = b.f(0)
}

object Test extends App {
  new Bar(23).f
  new Baz(23).f
}
