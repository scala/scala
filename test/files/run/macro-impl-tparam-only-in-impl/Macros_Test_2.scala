object Macros {
  def foo: Unit = macro Impls.foo[String]
}

object Test extends App {
  import Macros._
  foo
}