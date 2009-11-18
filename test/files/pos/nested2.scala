class C[A] {
  class D[B] {
  }
}

object Test {
  val x = new C[String]
  val y: C[String]#D[Int] = new x.D[Int]
}
