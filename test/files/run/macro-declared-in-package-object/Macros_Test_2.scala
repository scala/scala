package object Macros {
  def foo = macro Impls.foo
}

object Test extends App {
  import Macros._
  foo
}