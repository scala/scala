object Macros {
  type Foo = macro Impls.impl
}

object Test extends App {
  class D extends Macros.Foo
  val x: Macros.Foo = new D
}