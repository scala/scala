package tastytest

object TestRefinements extends Suite("TestRefinements") {

  class FooStringInt extends Refinements.Foo {
    type T = String
    type U = Int
    def foo = ("I am foo", 23)
  }

  trait FooString extends Refinements.FooT {
    type T = String
    def fooT = "I am foo"
  }

  trait FooInt extends Refinements.FooU {
    type U = Int
    def fooU = 23
  }

  test(assert((new Refinements.Bar[String, Int].bar(new FooStringInt)) == ("I am foo", 23)))

  test(assert((new Refinements.Baz[String, Int, FooStringInt].baz(new FooStringInt): (String, Int)) === ("I am foo", 23)))

  test(assert((new Refinements.Qux[String, Int, FooString with FooInt].qux( new FooString with FooInt ): (String, Int)) === ("I am foo", 23)))

  test(assert((new Refinements.Zot[String, Int].zot( new FooString with FooInt )) == ("I am foo", 23)))

}
