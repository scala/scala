class C[A] {
  class D[B] {
  }
}

object Test {
  val x = new C[String]
  val y: C[String]#D[int] = new x.D[int]
}
