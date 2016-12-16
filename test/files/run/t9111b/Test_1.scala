object Test extends App {
  val i = new A_1.Inner()
  val j = new i.Deeper()

  // TODO SI-9111: causes a NoSuchMethodError, should fail in mixed compilation.
  // assert(j.foo(new A_1.T()) == 1)

  // TODO SI-9111: fails in mixed compilation (found: A_1.P.T, required: A_1.T), should pass.
  // assert(j.foo(new A_1.P.T()) == 1)
}
