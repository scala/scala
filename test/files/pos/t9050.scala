final class Mu[F](val value: Any) extends AnyVal {
  def cata(f: F) {
    // crash
    ((y: Mu[F]) => y.cata(f))
    // crash
    def foo(x : Mu[F]) = x.cata(f)

    // // okay
    def x: Mu[F] = ???
    (() => x.cata(f))
    assert(true, cata(f))
  }
}
