import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo(x: Int) = macro Impls.refToFoo(42)
}

object Test extends App {
  import Macros._
  println(foo(42))
}