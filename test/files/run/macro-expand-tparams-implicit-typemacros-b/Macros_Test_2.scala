object Test extends App {
  type Foo[T](x: T) = macro Impls.foo[T]
  class D extends Foo(42)
  println(new D().tpe)
}