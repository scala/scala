object Test {
  new C
  new C1
  new C2

  class A[E[_]] { }
  class B[E[_]] extends A[B] { }  // B is depth 2 but A requires 1
  class C extends B { }

  class A1[E[F[G[_]]]] { }
  class B1[E[_]] extends A1[B1]   // B1 is depth 2 but A1 requires 3
  class C1 extends B1 { }

  class A2[E[_]] { }
  class B2[E] extends A2[B2] { }  // this one is correct
  class C2 extends B2 { }
}
