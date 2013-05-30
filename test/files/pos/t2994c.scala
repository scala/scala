object Test {
  trait Bar[X[_]]
  trait Qux[S[_] <: Bar[S], T]
  trait Baz[S[_] <: Bar[S]] {
    type Apply[T] = Qux[S,T]
  }
  trait Foo[/**/V[_] <: Bar[V]/**/] extends Bar[Baz[V]#Apply]
}
