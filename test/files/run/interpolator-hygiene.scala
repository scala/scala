// scalac: -Xsource:3
class C {
  class X(parts: Any*) {
    def s(args: Any*) = "hello, work"
  }
  object StringContext {
    def apply(parts: Any*) = new X(parts: _*)
  }
  def name = "Scala3"
  def test = s"hello, $name"
}
class D {
  import D.*
  class StringContext(parts: Any*) {
    def x(args: Any*) = "hello, work"
  }
  object StringContext {
    def apply(parts: Any*) = new StringContext(parts: _*)
  }
  def name = "Scala3"
  def test = x"hello, $name"
}
object D {
  implicit class x(val sc: StringContext) extends AnyVal {
    def x(args: Any*) = "hello, world"
  }
}
object Test extends App {
  assert(new C().test == "hello, Scala3")
  assert(new D().test == "hello, world")
}
