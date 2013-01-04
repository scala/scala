object Macros {
  type Foo(implicit x: Int) = macro Impls.impl
}

object Test extends App {
  implicit val x = 42
  class D extends Macros.Foo
  // val x: Macros.Foo = new D
}