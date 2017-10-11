class Functor[F[_]]
object Functor {
  val someF: Functor[F] forSome { type F[_] } = new Functor[Option]
}
