class B {

  type T = Int
  trait D

  // method parameter shadows some other type
  def foobar[D](in: D) = in.toString

  // type member's parameter shadows some other type
  type MySeq[D] = Seq[D]

  // class parameter shadows some other type
  class Foo[T](t: T) {
    // a type parameter shadows another type parameter
    def bar[T](w: T) = w.toString
  }

  // even deeply nested...
  class C[M[List[_]]]
  type E[M[List[_]]] = Int
  def foo[N[M[List[_]]]] = ???

  // ...but not between type parameters in the same list
  class F[A, M[L[A]]]         // no warning!
  type G[A, M[L[A]]] = Int    // no warning!
  def bar[A, N[M[L[A]]]] = ??? // no warning!
}
