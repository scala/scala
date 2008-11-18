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
  assert(Some(P(0)) == P.valueOf("A"))
  assert(Some(P.C) == P.valueOf("C"))
  assert(None == P.valueOf("Q"))

  assert(Some(Q(0)) == Q.valueOf("A"))
  assert(Some(Q.C) == Q.valueOf("C"))
  assert(None == Q.valueOf("Q"))

  assert(None == R.valueOf("A"))
  assert(None == R.valueOf("C"))
  assert(None == R.valueOf("Q"))
}
