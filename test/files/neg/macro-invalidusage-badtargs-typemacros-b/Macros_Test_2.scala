object Macros {
  type Foo[T](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends Foo[String, String](2)
  val x: Foo[String, String](2) = new C
}