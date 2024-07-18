//> using options -language:higherKinds,existentials -Xfatal-warnings
class Functor[F[_]]
object Functor {
  val someF: Functor[F] forSome { type F[_] } = new Functor[Option]
  val someG: Functor[G] forSome { type G[x] <: List[x] } = new Functor[::]
}
