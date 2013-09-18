object Q extends Enumeration {
  val A = Value("A")
  val B = Value("B")
  val C = Value("C")
}

object R extends Enumeration {
  val A, B, C = Value
}

object Test extends App {
  assert(Q(0) == Q.withName("A"))
  assert(Q.C == Q.withName("C"))

  assert(R(0) == R.withName("A"))
  assert(R.C == R.withName("C"))

  var failed = false
  try { Q.withName("x") } catch { case _: NoSuchElementException => failed = true }
  assert(failed)

}
