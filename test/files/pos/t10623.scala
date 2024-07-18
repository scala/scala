
//> using options -Xfatal-warnings -Xlint:unused
//

object `package` {
  def refl[A]: A Is A = ???
}

sealed trait Is[A, B] { ab =>
  def subst[F[_]](fa: F[A]): F[B]
  final def flip: B Is A = {
    type f[a] = a Is A
    subst[f](refl)
  }
}

