object Test extends App {
  val i = new A_1.Inner()
  val j = new i.Deeper()

//  assert(j.foo(new A_1.T()) == 1) // type mismatch, required: A_1.P.T, see neg/t9111
  assert(j.foo(new A_1.P.T()) == 1) // compiles, no error
}
