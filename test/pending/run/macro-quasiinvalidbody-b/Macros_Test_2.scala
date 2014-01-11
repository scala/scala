import scala.reflect.macros.blackbox.Context

object Macros extends ImplContainer {
  def foo(x: Any) = macro Impls.foo
}

object Test extends App {
  import Macros._
  println(foo(42))
}