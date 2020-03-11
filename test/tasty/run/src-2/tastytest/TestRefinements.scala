package tastytest

object TestRefinements extends Suite("TestRefinements") {

  class FooIntString extends Refinements.Foo {
    type T = String
    type U = Int
    def foo = ("I am foo", 23)
  }

  val b = new Refinements.Bar[String, Int]

  test(assert((b.bar(new FooIntString): (String, Int)) === ("I am foo", 23)))

}
