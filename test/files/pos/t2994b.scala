object Test {
  trait Bar[X[_]]
  trait Baz[S[_] <: Bar[S]] {
    type Apply[T]
  }
  trait Foo[V[_] <: Bar[V]] extends Bar[Baz[V]#Apply]
}