object Macros {
  val impls = new Impls
  def foo(x: Any) = macro impls.foo
}

object Test extends App {
  import Macros._
  foo(42)
}