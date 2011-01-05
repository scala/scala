object Test {
  def M[N[O[_]]] = ()
  type A[_] = Any
  type B[_[_]] = Any
  type C[_[_[_]]] = Any

  M[Any] // okay, Any is kind overloaded.

  M[AnyRef] // error, (AnyRef :: *) not kind-conformant to (N :: * -> * -> *)

  M[A] // error, (A :: (* -> *) not kind-conformant to (N :: * -> * -> *)

  M[B] // okay, (B :: (* -> * -> *) is kind-conformant to (N :: * -> * -> *)

  M[C] // error, (C :: (* -> * -> * -> *) not kind-conformant to (N :: * -> * -> *)
}