package tastytest.issue12420

object ShareLambda {

  class Foo[K[F[X] <: List[X]]] {
    def foo[F[X] <: List[X]](x: K[F]): String = x.toString()
  }

  // `F[X] <: List[X]` is structurally shared in TASTy and defined in `Foo.K`
  class Bar[F[X] <: List[X]] {
    override def toString(): String = "Bar"
  }

}
