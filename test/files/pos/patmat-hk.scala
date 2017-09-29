case class Foo[F[_]]()

case class APair[F[_], G[_], A](f: F[A], g: G[A])

object Test {
  Foo[({ type L[a] = (a, Int) })#L]() match {
    case Foo() => ()
  }

  APair[({ type L[a] = (Boolean, a) })#L, ({ type L[a] = a => Int })#L, String]((true, "two"), _.length) match {
    case APair((b, s), f) => ()
  }
}
