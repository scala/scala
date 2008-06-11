object Test {
  class Foo[T]
  type C[T] = Foo[_ <: T]
  val a: C[AnyRef] = new Foo[AnyRef]
}
