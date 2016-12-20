object Test {
  val e = new A_1.E()
  val f1: A_1.D.T = e.foo()  // no error
  val f2: A_1.C.T = e.foo()  // error: T is not a member of A_1.C (it's a non-static class)
  val f3: A_1.C#T = e.foo()  // mismatch, found: A_1.D.T

  val b1: A_1.C#T = e.bar()  // no error
  val b2: e.T     = e.bar()  // mismatch, bar returns A_1.C#T, not e.T - no path dependent types in Java
  val b3: A_1.D.T = e.bar()  // mismatch, found: A_1.C#T

  val m1: A_1.D.T = A_1.miz() // no error
  val m2: A_1.E.T = A_1.miz() // error: T is not a member of A_1.E - this selection is OK in Java, but not in Scala
  val m3: A_1.C#T = A_1.miz() // mismatch, found: A_1.D.T

  val p1: A_1.D.T = A_1.paz() // no error
  val p2: A_1.C#T = A_1.paz() // mismatch, found: A_1.D.T

  val t1: A_1.C#T = A_1.tux() // no error
  val t2: A_1.D.T = A_1.tux() // mismatch, found: A_1.C#T
}
