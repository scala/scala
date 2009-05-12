object P extends Enumeration(0, "A", "B", "C") { val A, B, C = Value }

object Q extends Enumeration {
  val A = Value("A")
  val B = Value("B")
  val C = Value("C")
}

object R extends Enumeration {
  val A, B, C = Value
}

object Test extends Application {
  assert(Some(P(0)) == P.withName("A"))
  assert(Some(P.C) == P.withName("C"))
  assert(None == P.withName("Q"))

  assert(Some(Q(0)) == Q.withName("A"))
  assert(Some(Q.C) == Q.withName("C"))
  assert(None == Q.withName("Q"))

  assert(None == R.withName("A"))
  assert(None == R.withName("C"))
  assert(None == R.withName("Q"))
}
