object Macros {
  type Foo(x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends Foo[String](2)
  val x: Foo[String](2) = new C
}