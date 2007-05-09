// contribution bug #461

object Test extends Application {
  def foo(ps: String*) = "Foo"
  case class X(p: String, ps: String*)
  def bar =
    X("a", "b") match {
      case X(p, ps @ _*) => foo(ps : _*)
    }
  println(bar)
}
