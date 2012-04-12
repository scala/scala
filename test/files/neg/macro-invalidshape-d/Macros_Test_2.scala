object Macros {
  def foo(x: Any) = {2; macro Impls.foo}
}

object Test extends App {
  import Macros._
  foo(42)
}