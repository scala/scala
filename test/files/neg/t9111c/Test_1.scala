object Test extends App {
  val i = new A_1.Inner()
  val j = new i.Deeper()

  assert(j.foo(new A_1.T()) == 1)   // TODO SI-9111: should not compile (need an A_1.P.T) (currently passes)
  assert(j.foo(new A_1.P.T()) == 1) // TODO SI-9111: should compile without error (currently fails, required: A_1.T)
}
