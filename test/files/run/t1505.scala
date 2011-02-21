object P extends Enumeration(0, "A", "B", "C") { val A, B, C = Value }

object Q extends Enumeration {
  val A = Value("A")
  val B = Value("B")
  val C = Value("C")
}

object R extends Enumeration {
  val A, B, C = Value
}

object Test extends App {
  assert(P(0) == P.withName("A"))
  assert(P.C == P.withName("C"))

  assert(Q(0) == Q.withName("A"))
  assert(Q.C == Q.withName("C"))
}
