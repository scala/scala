object Macros {
  def foo = macro Impls.foo[String]
}

object Test extends App {
  import Macros._
  foo
}