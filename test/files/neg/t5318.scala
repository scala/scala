class CompilerHang {
  trait TC[M[_]]
  trait S[A]

  implicit def tc[M[_]](implicit M0: TC[M]): TC[S] = null
  def breakage[F[_] : TC] = 0
  breakage  // type checker doesn't terminate, should report inference failure
}