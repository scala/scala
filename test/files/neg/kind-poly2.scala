object Test {

  // Checking edge cases that should not compile with kind-polymorphism

  trait X[F[_] <: AnyKind] { type L = F[Int]; def a: L = ??? }
  new X[Int] { }   // compiles and runs
  new X[Int] { }.a // the compiler dies
  new X[Either] { } // compiles and runs
  new X[Either] { }.a // doesn't compile (error: erroneous or inaccessible type)
  // dies with an assertion (different error than in the second case):
  new X[({ type l[X, Y] = X })#l] { }

}
