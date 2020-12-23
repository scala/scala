package tastytest

object TestMatchables extends Suite("TestMatchables") {

  test(assert(Matchables.foo == true))
  test(assert(Matchables.bar("hello") === "hello"))
  test(assert(new Matchables.baz(23).a === 23))
  test(assert(new Matchables.qux(5.0).a === 5.0))

}
