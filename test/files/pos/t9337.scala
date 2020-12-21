object Test {
  trait TypePreservingFn[T[X <: T[X]]]
  trait Validator[T, This <: Validator[T,This]]

  trait Foo[T] {
    type V[This <: Validator[T, This]] = Validator[T, This]
    val f: TypePreservingFn[V] = ???
  }

  class Bar[T] extends Foo[T] {
    val g: TypePreservingFn[V] = ???
  }
}
