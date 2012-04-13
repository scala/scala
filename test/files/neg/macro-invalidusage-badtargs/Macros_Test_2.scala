object Macros {
  def foo(x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  val s: String = foo[String](42)
}