object Test {
  trait Foo[S[_[_], _[_]]] extends Bar[S] {
    def m[F[_]](x: S[({ type G[A] = Bar[S] })#G, F]): Unit
  }
  trait Bar[S[_[_], _[_]]] {
    def m[F[_]](x: S[({ type G[A] = Bar[S] })#G, F]): Unit
  }
}
