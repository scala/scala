object Test extends App {
  val i = new A_1.Inner()
  val j = new i.Deeper()

  assert(j.foo(new A_1.T()) == 1)   // type mismatch, required: A_1.P.T
  assert(j.foo(new A_1.P.T()) == 1) // compiles, no error, see run/t9111b
}
