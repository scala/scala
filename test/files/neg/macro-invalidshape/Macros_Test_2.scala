object Macros {
  def foo1(x: Any) = macro 2
  def foo2(x: Any) = macro Impls.foo(null)(null)
  def foo3(x: Any) = macro {2; Impls.foo}
}

object Test extends App {
  import Macros._
  foo1(42)
  foo2(42)
  foo3(42)
}