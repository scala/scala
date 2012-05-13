class CompilerHang {
  trait TC[M[_]]
  trait S[A]

  class C[M[_]] {
    type TCM = TC[M]
  }

  // A nefarious implicit, to motivate the removal of `&& sym.owner.isTerm` from
  // `isFreeTypeParamNoSkolem`.
  implicit def tc[x[_], CC[x[_]] <: C[x]](implicit M0: CC[x]#TCM): CC[x]#TCM = null
  def breakage[F[_] : TC] = 0
  breakage // type checker doesn't terminate, should report inference failure
}
