object Macros {
  def foo[T](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  val s: String = foo[String, String](42)
}