class Macros extends MacroHelpers {
  def foo(x: Any) = macro Impls.foo
}

object Test extends App {
  println(new Macros().foo(42))
}