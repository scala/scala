object Macros {
  type Foo(x: Int)(y: Int) = macro Impls.impl
}

object Test extends App {
  class D extends Macros.Foo(2)(3)
  val x: Macros.Foo(2)(3) = new D
}