object Macros {
  type Foo(x: Any) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends Foo(zzz)
  val x: Foo(zzz) = null
}