class Functor[F[_]]
object Functor {
  val someF: Functor[F] forSome { type F[x] >: List[x] } = new Functor[Option]
}
