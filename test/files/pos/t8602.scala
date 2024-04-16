//> using options -language:higherKinds
//
object Test {
  case class Foo[CC[_], D <: CC[Int]](d: D, cc: CC[Int])
  Foo(Nil, List(1, 2, 3))

  class H[F[_]]
  def g[F[_], T, FT <: F[T]](h: H[F]) = 1
  g(new H[Set])
}
