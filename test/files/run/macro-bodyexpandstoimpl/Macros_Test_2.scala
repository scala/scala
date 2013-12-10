import scala.language.experimental.macros

object Macros {
  def foo(x: Int): Int = macro Impls.refToFoo(42)
}

object Test extends App {
  import Macros._
  println(foo(42))
}