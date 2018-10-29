import scala.language.higherKinds

object Test {
  trait HKT[F[_]]

  trait A1[S[_]] {      type T } // HKT param
  trait A2 { type S   ; type T } // non-HKT member
  trait A3 { type S[_]; type T } // HKT member

  // "Aux"-style aliases for fixing member types: "ST" fixes S and T, "T" fixes just T (where applicable)
  object A1 { type ST[ S[_], _T] = A1[S] {                    type T = _T }                                  }
  object A2 { type ST[_S   , _T] = A2    { type S    = _S   ; type T = _T }; type T[_T] = A2 { type T = _T } }
  object A3 { type ST[_S[_], _T] = A3    { type S[U] = _S[U]; type T = _T }; type T[_T] = A3 { type T = _T } }

  // HKT derivations for aliases above, always with wildcard `T` in rightmost position, for partial unification
  implicit def a1[S[_]]: HKT[({ type F[x] = A1.ST[S, x] })#F] = ???
  implicit def a2[S   ]: HKT[({ type F[x] = A2.ST[S, x] })#F] = ???
  implicit def a3[S[_]]: HKT[({ type F[x] = A3.ST[S, x] })#F] = ???
  implicit def a2t     : HKT[A2. T      ] = ???
  implicit def a3t     : HKT[A3. T      ] = ???

  implicitly[HKT[({ type F[x] = A1.ST[List, x] })#F]] // HKT-param
  implicitly[HKT[({ type F[x] = A2.ST[Char, x] })#F]] // non-HKT member
  implicitly[HKT[({ type F[x] = A3.ST[List, x] })#F]] // HKT-member
  implicitly[HKT[A2.T]]
  implicitly[HKT[A3.T]]
}
