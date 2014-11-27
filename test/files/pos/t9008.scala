trait Monad[M[_]]

object Test {
  def x: Monad[M forSome { type M[_] }] = ???
}
